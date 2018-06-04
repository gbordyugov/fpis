import java.util.concurrent.{ExecutorService, Future, TimeUnit}

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

    def map2[A, B, C](a: Par[A], b: Par[B])
      (f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }
}
