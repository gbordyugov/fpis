package fpis.chapter13.BetweenTwoAndThree

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

sealed trait Free2[F[_],A]
case class Return2[F[_],A](a: A) extends Free2[F,A]
case class Suspend2[F[_],A](fa: F[Free2[F,A]]) extends Free2[F,A]

object Free2 {
  def from3[F[_],A](a: Free3[F,A])
    (implicit F: Functor[F]): Free2[F,A] = a match {
    case Return3(a)     => Return2(a)
    case Suspend3(fa)   => Suspend2(F.map(fa)(Return2(_)))
    case FlatMap3(a, f) => ???
  }
}

sealed trait Free3[F[_],A]
case class Return3[F[_],A](a: A) extends Free3[F,A]
case class Suspend3[F[_],A](fa: F[A]) extends Free3[F,A]
case class FlatMap3[F[_],A,B](a: Free3[F,A], f: A => Free3[F,B]) extends Free3[F,B]

object Free3 {
  def from2[F[_],A](a: Free2[F,A]): Free3[F,A] = a match {
    case Return2(a)  => Return3(a)
    case Suspend2(a) => ???
  }
}
