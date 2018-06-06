import java.util.concurrent.{ExecutorService, Future, TimeUnit,
  Callable}

object Parallel {

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
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  object Par {
    def unit[A](a: A): Par[A] =
      (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))


    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }


    /*
     * Exercise 7.3
     */

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
      val fa = a(es)
      val fb = b(es)
      Map2Future(fa, fb)(f)
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

      private def compute(timeInMSec: Long): C = cache match {
        case Some(c) => c
        case None    =>
          /*
           * that's really naive, it can happen that one leg takes
           * longer to evaluate than the other one
           */
          val a = fa.get(timeInMSec/2, TimeUnit.MILLISECONDS)
          val b = fb.get(timeInMSec/2, TimeUnit.MILLISECONDS)
          val result = f(a, b)
          cache = Some(result)
          result
      }
    }

    def map2Old[A, B, C](a: Par[A], b: Par[B])
      (f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }


    def fork[A](a: Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        /*
         * this will block the caller's thread
         */
        def call = a(es).get
      })

    /*
     * Exercise 7.4
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
        map2(indicators, unit(as)) { (i, a) => {
            i.zip(a).filter(_._1).map(_._2)
          }
        }
      }

    def equal[A](e: ExecutorService)
      (p: Par[A], q: Par[A]): Boolean =
      p(e).get == q(e).get
  }
}
