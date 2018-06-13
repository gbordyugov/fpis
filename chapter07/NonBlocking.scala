import java.util.concurrent.ExecutorService

object NonBlocking {
  sealed trait Future[A] {
    /* private[parallelism] */ def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]
}
