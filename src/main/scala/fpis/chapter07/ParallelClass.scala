
package fpis.chapter07class

import annotation.tailrec
import java.util.concurrent.{ExecutorService, Future, TimeUnit,
Callable, Executors}

object Par {
  /*
   * Exercise 7.1
   */
  def map2First[A, B, C](a: Par[A], b: Par[B])
  (f: (A, B) => C): Par[C] = ???


  /*
   * Exercise 7.2 skipped
   */


  /*
   * Par[A] is parameterised by an ExecutorService
   */
  case class Par[A](run: ExecutorService => Future[A]) { self =>
    def apply(es: ExecutorService) = run(es)

    def map2[B, C](that: Par[B])(f: (A, B) => C): Par[C] = 
      Par[C](es => {
        val x = this(es)
        val y = that(es)
        Map2Future(x, y)(f)
      })

    def map[B](f: A => B): Par[B] =
      self.map2(unit(()))((a, _) => f(a))

    def fork = Par[A](es =>
      es.submit(new Callable[A] {
        /*
         * this will block the caller's thread
         */
        def call = {
          println("fork")
          self(es).get
        }
      }))

  }

  def unit[A](a: A): Par[A] =
    Par[A](es => UnitFuture(a))

  def lazyUnit[A](a: => A): Par[A] =
    unit(a).fork


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  /*
   * for Exercise 7.3, cheated from the companion book
   */
  private case class Map2Future[A, B, C](fa: Future[A],
    fb: Future[B])(f: (A, B) => C) extends Future[C] {

    @volatile
    var cache: Option[C] = None
    def isDone = ! cache.isEmpty
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit) =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))
    def isCancelled = fa.isCancelled || fb.isCancelled
    def cancel(evenIfRunning: Boolean) =
      fa.cancel(evenIfRunning) && fb.cancel(evenIfRunning)

    private def compute(timeMs: Long): C = cache match {
      case Some(c) => c
      case None    =>
        /*
         * that's really naive, it can happen that one leg takes
         * longer to evaluate than the other one
         */
        val startMs = System.currentTimeMillis()
        val a = fa.get(timeMs, TimeUnit.MILLISECONDS)
        val usedMs = System.currentTimeMillis() - startMs
        val availableMs = timeMs - usedMs
        val b = fb.get(availableMs, TimeUnit.MILLISECONDS)
        val result = f(a, b)
        cache = Some(result)
        result
    }
  }

  /*
   * Exercise 7.4
   */

  /*
   * this one takes a function and turns it into another function
   * that does its dirty job in the background
   */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))



  /*
   * Exercise 7.5
   */

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    val zero: Par[List[A]] = unit(List.empty)
    ps.foldRight(zero)((a, b) => a.map2( b)(_ :: _))
  }


  /*
   * here and below, we fork first in order to be able to return
   * immediately. After this computation will be passed to run(),
   * it will spawn additional ps.length threads
   */
  def parMap[A, B](ps: List[A])(f: A=>B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs).fork
  }


  /*
   * Exercise 7.6
   */

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val indicators: Par[List[Boolean]] = parMap(as)(f)
    indicators.map2(unit(as)) { (i, a) => {
      i.zip(a).filter(_._1).map(_._2)
    }
    }.fork
  }

  def equal[A](e: ExecutorService) (p: Par[A], q: Par[A]): Boolean =
    p(e).get == q(e).get


  /*
   * Exercise 7.7
   *
   * map(y)(id) == y // apply map(_)(f compose g) on both sides
   * map(map(y)(id))(f compose g) = map(y)(f compose g)
   * ...
   * skipping for now
   *
   */



  /*
   * Exercise 7.8
   *
   * if we fork many enough times, we'll run of locked Threads
   */


  /*
   * Exercise 7.9
   *
   * see Exercise 7.8
   *
   */


  def badFork[A](fa: => Par[A]): Par[A] =
    // doesn't really do any forking
  Par[A](es => fa(es))


  /*
   * actually it's a delay - it avoid evaluating until the actual
   * result is neeed
   */
  def delay[A](fa: => Par[A]): Par[A] =
    Par[A](es => fa(es))


  /*
   * Exercise 7.11
   */

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    Par[A](es => {
      val k = n(es).get
      choices(k)(es)
    })

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map{if (_) 0 else 1})(List(t, f))


  /*
   * Exercise 7.12
   */

  def choiceMap[K, V](key: Par[K])(chs: Map[K, Par[V]]): Par[V] =
    Par[V](es => {
      val k = key(es).get
      chs(k)(es)
    })


  /*
   * Exercise 7.13
   */

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    Par[B](es => {
      val a = pa(es).get
      choices(a)(es)
    })

  def choiceByChooser[A, B](cond: Par[Boolean])
  (t: Par[A], f: Par[A]): Par[A] = chooser(cond){if (_) t else f}

  def choiceNByChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  /*
   * my favourite one!
   */
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par[B](es => {
    val a = pa(es).get
    f(a)(es)
  })

  /*
   * Exercise 7.14
   */

  def join[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  /*
   * that's double-recursive, never use it
   */
  def flatMapByJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(pa.map(f))

  def flatMapByMap2[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(pa.map2(unit(1)){(a, b) => f(a)})

  def map2ByFlatMap[A, B, C](pa: Par[A], pb: Par[B])
    (f: (A, B) => C): Par[C] =
    flatMap(pa){a => flatMap(pb){b => unit(f(a, b))}}
}

object Examples {
  val es = Executors.newFixedThreadPool(2)

  val p1 = Par.unit(1)
  val p2 = Par.lazyUnit(1)

  @tailrec
  def merge(a: List[Int], b: List[Int], acc: List[Int] = List()): List[Int] =
    (a, b) match {
      case (Nil,   Nil)       => acc.reverse
      case (a::as, Nil)       => merge(as, Nil, a +: acc)
      case (Nil, b::bs)       => merge(Nil, bs, b +: acc)
      case (a::as, b::bs)     =>
        if (a <= b)
          merge(as, b::bs, a +: acc)
        else
          merge(a::as, bs, b +: acc)
    }
  def mergeSort(s: List[Int]): Par.Par[List[Int]] = {
    val length = s.length
    if (length < 2)
      Par.unit(s)
    else {
      val (l, r) = s.splitAt(length/2)
      println((l, r))
      val (lSorted, rSorted) = (mergeSort(l), mergeSort(r))
      lSorted.map2(rSorted)((a, b) => merge(a, b)).fork
    }
  }
}
