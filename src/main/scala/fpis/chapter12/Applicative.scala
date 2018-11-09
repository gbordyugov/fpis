package fpis.chapter12

import scala.language.higherKinds

import fpis.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]


  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /*
   * Exercise 12.1
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(x => x)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    val lst: List[F[A]] = (1 to n).toList.map(i => fa)
    sequence(lst)
  }

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  /*
   * Exercise 12.2
   *
   * apply can be a primitve though
   */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def mapViaApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2ViaApplyAndUnit[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val g = unit(f.curried)
    apply(apply(g)(fa))(fb)
  }

  /*
   * Exercise 12.3
   */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val g = unit(f.curried)
    apply(apply(apply(g)(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
    (f: (A, B, C, D) => E): F[E] = {
    val g = unit(f.curried)
    apply(apply(apply(apply(g)(fa))(fb))(fc))(fd)
  }

  /*
   * Exercise 12.8
   */
  def product[G[_]]: Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      type T[A] = (F[A], G[A])
      def unit[A](a: => A): T[A] = ???

      def map2[A, B, C](ma: T[A], mb: T[B])(f: (A, B) => C): T[C] = ???
    }
}

object ApplicativeStream {
  import fpis.chapter05.Stream

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.constant(a)

    def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a.zip(b).map(f.tupled)
  }

  /*
   * Exercise 12.4
   *
   * Applicative.sequence() for Stream performs a transposition
   */
}

/*
 * Exercise 12.5
 */
object EitherMonad {
  import fpis.chapter11.Monad

  def eitherMonad[E] = new Monad[({type f[x] = Either[E,x]})#f] {
    def unit[A](a: A): Either[E,A] = Right[E,A](a)
    def flatMap[A,B](ma: Either[E,A])(f: A => Either[E,B]): Either[E,B] =
      ma match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
  }
}

sealed trait Validation[+E,+A]

case class Failure[E](head: E,
  tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

/*
 * Exercise 12.6
 */
object ValidationApplicative {
  def validationApplicative[E] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      def unit[A](a: => A): Validation[E,A] = Success[A](a)
      def map2[A,B,C](ma: Validation[E,A],
        mb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
        (ma, mb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (_, f@Failure(_, _)) => f
          case (f@Failure(_, _), _) => f
        }
    }
}

/*
 * Exercise 12.7 skipped
 */
