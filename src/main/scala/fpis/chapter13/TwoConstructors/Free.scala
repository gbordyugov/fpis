package fpis.chapter13.TwoConstructors

import scala.language.higherKinds

import fpis.chapter13.{Monad, Functor}

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B])
    (implicit F: Functor[F]): Free[F,B] = this match {
    case Return(a)  => f(a)
    case Suspend(fa) => Suspend(F.map(fa)(ffa => ffa.flatMap(f)))
  }

  def map[B](f: A => B)(implicit F: Functor[F]): Free[F,B] =
    flatMap(x => Return(f(x)))
}

case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F,A]]) extends Free[F,A]

object Free {
  def freeMonad[F[_]](implicit F: Functor[F]):
    Monad[({type t[x] = Free[F,x]})#t] =
    new Monad[({type t[x] = Free[F,x]})#t] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A,B](a: Free[F,A])(f: A => Free[F,B]): Free[F,B] =
        a.flatMap(f)
    }

  def run[F[_],A](f: Free[F,A])(implicit F: Monad[F]): F[A] = f match {
    case Return(a)   => F.unit(a)
    case Suspend(fa) => F.join(F.map(fa)(a => run(a)))
  }
}
