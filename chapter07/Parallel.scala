import java.util.concurrent.{ExecutorService, Future}

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
}
