package fpis.chapter13.TwoConstructors

import scala.language.higherKinds

import fpis.chapter13.Monad

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
    case Return(a)  => f(a)
    case Suspend(f) => ???
  }

  def map[B](f: A => B): Free[F,B] =
    flatMap(x => Return(f(x)))
}

case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F,A]]) extends Free[F,A]

object Free {
}
