package fpis.chapter13.BetweenTwoAndThree

import scala.language.higherKinds

sealed trait Free2[F[_],A]
case class Return2[F[_],A](a: A) extends Free2[F,A]
case class Suspend2[F[_],A](fa: F[Free2[F,A]]) extends Free2[F,A]


sealed trait Free3[F[_],A]
case class Return3[F[_],A](a: A) extends Free3[F,A]
case class Suspend3[F[_],A](fa: F[A]) extends Free3[F,A]
case class FlatMap3[F[_],A,B](a: Free3[F,A], f: A => Free3[F,B]) extends Free3[F,B]
