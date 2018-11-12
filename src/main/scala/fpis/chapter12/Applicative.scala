package fpis.chapter12

import scala.language.higherKinds

import fpis.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] { self =>
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
  def product[G[_]](G: Applicative[G]):
      Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      val F = self

      type Pair[A] = (F[A], G[A])

      def unit[A](a: => A): Pair[A] = (F.unit(a), G.unit(a))

      def map2[A, B, C](ma: Pair[A], mb: Pair[B])
        (f: (A, B) => C): Pair[C] =
        (F.map2(ma._1, mb._1)(f), G.map2(ma._2, mb._2)(f))
    }

  /*
   * Exercise 12.9
   */
  def compose[G[_]](G: Applicative[G]):
      Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      val F = self

      type Composition[A] = F[G[A]]

      def unit[A](a: => A): Composition[A] = F.unit(G.unit(a))

      // worth a blog post how I was figuring it out
      def map2[A, B, C](fga: Composition[A], fgb: Composition[B])
        (f: (A, B) => C): Composition[C] =
        F.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }

  /*
   * Exercise 12.10 skipped
   */

  /*
   * Exercise 12.11
   *
   * it's not possible, but monad transformers help when the inner type
   * is know
   */

  /*
   * Exercise 12.12
   */
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    val lst: List[(K, F[V])] = ofa.toList
    val zero: F[Map[K,V]] = unit(Map[K,V]())

    lst.foldLeft(zero) { case (acc: F[Map[K,V]], (k: K, fv: F[V])) =>
      map2(acc, fv) { (acc: Map[K,V], v: V) =>
        acc + (k -> v)}
    }
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


trait Traversable[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])
    (f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object TraversableInstances {
  /*
   * Exercise 12.13
   */
  val traversableOption = new Traversable[Option] {
    def map[A,B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)

    override def sequence[G[_]: Applicative, A](oa: Option[G[A]]):
        G[Option[A]] = {
      val app = implicitly[Applicative[G]]
      oa match {
        case None     => app.unit(None)
        case Some(ga) => app.map(ga)(Some(_))
      }
    }
  }

  val traversableList = new Traversable[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as.map(f)

    override def sequence[G[_]: Applicative, A](as: List[G[A]]):
        G[List[A]] = {
      val app = implicitly[Applicative[G]]
      as.foldRight(app.unit(List(): List[A])) {(ga, gla) =>
        app.map2(ga, gla)((_ :: _))
      }
    }
  }

  val traversableTree = new Traversable[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = ???

    override def sequence[G[_]: Applicative, A](tree: Tree[G[A]]):
        G[Tree[A]] = tree match {
      case Tree(h, Nil) => ???
      case Tree(h, t)   => ???
    }
  }
}
