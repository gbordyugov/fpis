package fpis.chapter11

import fpis.chapter05.Stream
import fpis.chapter06.State
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

  /*
   * Exercise 11.3
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List(): List[A])) { (e, lst) => map2(e, lst)(_ :: _) }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  /*
   * Exercise 11.4
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    val lst: List[F[A]] = (1 to n).toList.map(i => ma)
    sequence(lst)
  }

  /*
   * Exercise 11.5
   *
   * for M = List, the above function becomes
   * def replicateM[A](n: Int, as: List[A]): List[List[A]]
   * which returns a list containing n copies of as
   *
   * for M = Option, the above function becomes
   * def replicateM[A](n: Int, oa: Option[A]): Option[List[A]]
   * so when oa is None, None is returned, otherwise Some(List[A])
   */

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  /*
   * Exercise 11.6
   * first ugly version
   */
  def filterM_[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil     => unit(Nil)
    case a :: as => {
      val rest: F[List[A]] = filterM(as)(f)
      val fa: F[Boolean] = f(a)
      flatMap(fa) { b =>
        if (b)
          map(rest) { as => a :: as}
        else
          rest
      }
    }
  }

  /*
   * Exercise 11.6
   * trying to come up with a nicer solution
   */
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(Nil: List[A])) { (a: A, rest: F[List[A]]) =>
      flatMap(f(a))(if (_) map(rest)(r => a::r) else rest)
    }
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

  val optionMonad = new Monad[Option] {
    def unit[A](a: A) = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    import fpis.chapter05.Stream._
    def unit[A](a: A) = Stream(a)
    def flatMap[A,B](sa: Stream[A])(f: A => Stream[B]) = sa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: A) = List(a)
    def flatMap[A,B](as: List[A])(f: A => List[B]) = as.flatMap(f)
  }

  /*
   * Exercise 11.2
   */

  type IntState[A] = State[Int, A]

  val intStateMonad = new Monad[IntState] {
    import fpis.chapter06.State.{unit => sUnit}
    type StateType = Int
    def unit[A](a: A) = sUnit[StateType,A](a)
    def flatMap[A,B](sa: IntState[A])(f: A => IntState[B]): IntState[B] =
      sa.flatMap(f)
  }
}
