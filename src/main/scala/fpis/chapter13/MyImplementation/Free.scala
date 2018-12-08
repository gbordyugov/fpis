package fpis.chapter13.MyImplementation
import fpis.chapter13.Monad

import scala.language.higherKinds

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B]): Free[F,B] =
    FlatMap(this, f)
  def map[B](f: A => B): Free[F,B] =
    flatMap(x => Return(f(x)))
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](fa: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](fa: Free[F,A], f: A => Free[F,B])
    extends Free[F,B]

trait Translate[F[_],G[_]] {
  def apply[A](a: F[A]): G[A]
}

object Free {
  type ~>[F[_],G[_]] = Translate[F,G]

  def run[F[_],A](f: Free[F,A])(implicit F: Monad[F]): F[A] = f match {
    case Return(a)     => F.unit(a)
    case Suspend(fa)   => fa
    case FlatMap(x, f) => x match {
      case Return(a)     => run(f(a))
      case Suspend(fa)   => F.flatMap(fa)(x => run(f(x)))
      case FlatMap(y, g) => run(y.flatMap(y => g(y).flatMap(f)))
    }
  }

  /*
   * here the rub is that F is not a monad anymore, so we have to resort
   * to using the monad properties of G
   */
  def runFree[F[_],G[_],A](f: Free[F,A])(t: F~>G)
    (implicit G: Monad[G]): G[A] = f match {
    case Return(a)     => G.unit(a)
    case Suspend(fa)   => t(fa)
    case FlatMap(x, f) => ???
  }
}
