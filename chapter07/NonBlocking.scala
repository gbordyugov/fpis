import java.util.concurrent.ExecutorService
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference

object NonBlocking {
  sealed trait Future[A] {
    /* private[parallelism] */ def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      a => ref.set(a)
      latch.countDown
    }
    ref.get
  }
}
