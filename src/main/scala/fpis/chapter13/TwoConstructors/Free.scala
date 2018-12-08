package fpis.chapter13.TwoConstructors

import scala.language.higherKinds

import fpis.chapter13.Monad

sealed trait Free[F[_],A]
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F,A]]) extends Free[F,A]
