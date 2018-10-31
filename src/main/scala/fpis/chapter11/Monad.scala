package fpis.chapter11

import fpis.chapter07.Par.Par
import fpis.chapter08.Gen
import fpis.chapter09.Parsers

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A=>B): F[B]

  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fa) => map(fa)(Right(_))
    }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](fa: F[A])(f: A=>F[B]): F[B]

  def map[A,B](fa: F[A])(f: A=>B): F[B] =
    flatMap(fa) { a => unit(f(a)) }

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B)=>C): F[C] =
    flatMap(fa) { a => map(fb) { b => f(a, b) } }
}

object Monad {
  val genMonad = new Monad[Gen] {
    import fpis.chapter08.Gen._

    def unit[A](a: A) = Gen.unit(a)
    def flatMap[A,B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  /*
   * Exercse 11.1
   */
  val parMonad = new Monad[Par] {
    import fpis.chapter07.Par.{unit => pUnit, flatMap => pFlatMap}

    def unit[A](a: A) = pUnit(a)
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]) = pFlatMap(pa)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: A) = p.succeed(a)
    def flatMap[A,B](pa: P[A])(f: A => P[B]) = p.flatMap(pa)(f)
  }
}
