package fpis.chapter13.BetweenTwoAndThree

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B]

  def map[A,B](fa: F[A])(f: A=>B): F[B] =
    flatMap(fa)(x => unit(f(x)))
}

sealed trait Free2[F[_],A] {
  def flatMap[B](f: A => Free2[F,B])
    (implicit F: Functor[F]): Free2[F,B] = this match {
    case Return2(a)  => f(a)
    case Suspend2(fa) => Suspend2(F.map(fa)(ffa => ffa.flatMap(f)))
  }

  def map[B](f: A => B)(implicit F: Functor[F]): Free2[F,B] =
    flatMap(x => Return2(f(x)))
}

case class Return2[F[_],A](a: A) extends Free2[F,A]
case class Suspend2[F[_],A](fa: F[Free2[F,A]]) extends Free2[F,A]

object Free2 {
  def free2Monad[F[_]](implicit F: Functor[F]) =
    new Monad[({type t[x] = Free2[F,x]})#t] {
      def unit[A](a: => A): Free2[F,A] = Return2(a)
      def flatMap[A,B](fa: Free2[F,A])(f: A => Free2[F,B]): Free2[F,B] =
        fa.flatMap(f)
    }

  def from3[F[_],A](a: Free3[F,A])
    (implicit F: Functor[F]): Free2[F,A] = a match {
    case Return3(a)     => Return2(a)
    case Suspend3(fa)   => Suspend2(F.map(fa)(Return2(_)))
    case FlatMap3(a, f) => ???
  }
}

sealed trait Free3[F[_],A] {
  def flatMap[B](f: A => Free3[F,B]) =
    FlatMap3(this, f)
  def map[B](f: A => B): Free3[F,B] =
    flatMap(a => Return3(f(a)))
}

case class Return3[F[_],A](a: A) extends Free3[F,A]
case class Suspend3[F[_],A](fa: F[A]) extends Free3[F,A]
case class FlatMap3[F[_],A,B](a: Free3[F,A], f: A => Free3[F,B])
    extends Free3[F,B]

object Free3 {
  def free3Monad[F[_]] =
    new Monad[({type t[x] = Free3[F,x]})#t] {
      def unit[A](a: => A): Free3[F,A] = Return3(a)
      def flatMap[A,B](fa: Free3[F,A])(f: A => Free3[F,B]): Free3[F,B] =
        fa.flatMap(f)
    }

  def from2[F[_],A](a: Free2[F,A])
    (implicit F: Functor[F]): Free3[F,A] = a match {
    case Return2(a)                  => Return3(a)
    case Suspend2(fa: F[Free2[F,A]]) => {
      val tmp: F[Free3[F,A]] = F.map(fa)(from2(_))
      ???
    }
  }
}
