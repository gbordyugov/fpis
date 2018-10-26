package fpis.chapter07

import java.util.concurrent.{ExecutorService, Future, TimeUnit, Callable}

case class Result[+A](value: A, forkDepth: Int = 0) {
  def incDepth: Result[A] = copy(forkDepth=forkDepth+1)
}

object Result {
  def apply[A](value: A, res1: Result[Any], res2: Result[Any]): Result[A] =
    Result(value=value, forkDepth=res1.forkDepth.max(res2.forkDepth))
}


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
  type Par[A] = ExecutorService => Future[Result[A]]

  def run[A](s: ExecutorService)(a: Par[A]): Future[Result[A]] = a(s)

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(Result(value=a, forkDepth=0))

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  /*
   * This future doesn't really do anything useful --
   * it's just a state wrapped in Future
   */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class MapFuture[A,B](future: Future[A])(f: A=>B) extends Future[B] {
    def isDone = future.isDone
    def get = f(future.get)
    def get(timeout: Long, units: TimeUnit) = f(future.get(timeout, units))
    def isCancelled = future.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = future.cancel(evenIfRunning)
  }


  /*
   * Exercise 7.3
   */

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      /*
       * it creates two futures by running `a` and `b` and constructs
       * the result future out of those two
       */
      val fa = a(es)
      val fb = b(es)
      Map2Future(fa, fb) { case (Result(v1, d1), Result(v2, d2)) =>
        Result(f(v1, v2), d1.max(d2))
      }
    }

  /*
   * for Exercise 7.3, cheated from the companion book
   *
   * this future has a state `cache` which is an Option
   * it is initially None, but once the result has been computed,
   * it becomes Some(_)
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

  def map2Old[A, B, C](a: Par[A], b: Par[B])
  (f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(Result(f(af.get.value, bf.get.value), af.get, bf.get))
  }


  /*
   * fork() submits a new Callable to the ExecutorService
   * this new Callable calls the Par to be forked with es
   * and blockingly _waits_ until it's completed
   * thus blocking the caller's thread
   */
  def fork[A](a: Par[A]): Par[A] = (es: ExecutorService) => {
    val f: Future[Result[A]] = es.submit(new Callable[Result[A]] {
      def call = {
        /*
         * this will block the caller's thread
         */
        a(es).get
      }
    })
    MapFuture(f)(_.incDepth)
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


  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))


  /*
   * Exercise 7.5
   */

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    val zero: Par[List[A]] = unit(List.empty)
    ps.foldRight(zero)((a, b) => map2(a, b)(_ :: _))
  }


  /*
   * here and below, we fork first in order to be able to return
   * immediately. After this computation will be passed to run(),
   * it will spawn additional ps.length threads
   */
  def parMap[A, B](ps: List[A])(f: A=>B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  /*
   * Exercise 7.6
   */

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val indicators: Par[List[Boolean]] = parMap(as)(f)
      map2(indicators, unit(as)) { (i, a) =>
        i.zip(a).filter(_._1).map(_._2)
      }
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
  es => fa(es)


  /*
   * actually it's a delay - it avoid evaluating until the actual
   * result is neeed
   */
  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)


  /*
   * Exercise 7.11
   */

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val k = run(es)(n).get.value
      choices(k)(es)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond){if (_) 0 else 1})(List(t, f))


  /*
   * Exercise 7.12
   */

  def choiceMap[K, V](key: Par[K])(chs: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get.value
      chs(k)(es)
    }


  /*
   * Exercise 7.13
   */

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get.value
      choices(a)(es)
    }

  def choiceByChooser[A, B](cond: Par[Boolean])
  (t: Par[A], f: Par[A]): Par[A] = chooser(cond){if (_) t else f}

  def choiceNByChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  /*
   * my favourite one!
   */
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get.value
    f(a)(es)
  }

  /*
   * Exercise 7.14
   */

  def join[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  /*
   * that's double-recursive, never use it
   */
  def flatMapByJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

  def flatMapByMap2[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map2(pa, unit(1)){(a, b) => f(a)})

  def map2ByFlatMap[A, B, C](pa: Par[A], pb: Par[B])
    (f: (A, B) => C): Par[C] =
    flatMap(pa){a => flatMap(pb){b => unit(f(a, b))}}
}
