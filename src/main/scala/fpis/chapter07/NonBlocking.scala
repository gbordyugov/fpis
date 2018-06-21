package fpis.chapter07

import java.util.concurrent.{ExecutorService, Callable}
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


  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(callback: A => Unit): Unit =
        callback(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(callback: A => Unit): Unit =
        eval(es)(a(es)(callback))
    }
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
      }
    )

  /*
   * Exercise 7.10
   */

  object Exercise710 {
    sealed trait Future[A] {
      def apply(callback: A => Unit)(failure: Exception => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a)
        latch.countDown
      } { e =>
        ???
        latch.countDown
      }
      ref.get
    }
  }
}
