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
  }
}