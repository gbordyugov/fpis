package fpis.chapter15.ExtensibleProcess

import scala.language.higherKinds

import fpis.chapter13.Monad

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]
}
