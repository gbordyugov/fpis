package fpis.chapter13.MyImplementation

import scala.language.higherKinds

sealed trait Free[F[_],A]
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](fa: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](fa: Free[F,A])(f: A => Free[F,B])
    extends Free[F,B]
