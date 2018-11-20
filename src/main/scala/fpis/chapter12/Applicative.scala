package fpis.chapter12

import scala.language.higherKinds

import fpis.chapter06.State
import fpis.chapter11.Functor
import fpis.chapter11.Monad
import fpis.chapter11.IntMonadState.stateMonad

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

    lst.foldLeft(zero) { case (acc, (k, fv)) =>
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
    def unit[A](a: => A): Either[E,A] = Right[E,A](a)
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


trait Traverse[F[_]] extends Functor[F] { self =>
  import fpis.chapter06.State.{get, set}

  def traverse[G[_]: Applicative, A, B](fa: F[A])
    (f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  /*
   * Exercise 12.14
   *
   * this proves that every Traversable is a Functor
   */
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    type Id[A] = A
    implicit val idApplicative = new Applicative[Id] {
      def unit[A](a: => A): Id[A] = a
      def map2[A,B,C](a: Id[A], b: Id[B])(f: (A, B) => C): Id[C] =
        f(a, b)
    }
    traverse[Id,A,B](fa)(f)
  }

  // case class State[S, +A](run: S => (A, S))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = {
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(stateMonad)
  }

  def zipWithIndexOld[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => ( for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  def toListOld[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => ( for {
      as <- get[List[A]]
      _  <- set(a :: as)
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => ( for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def ziwWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /*
   * Exercise 12.16
   */
  def reverse[A](fa: F[A]): F[A] = {
    val l = toList(fa).reverse
    mapAccum(fa, l)((_, as) => (as.head, as.tail))._1
  }

  /*
   * Exercise 12.17
   */
  def foldLeft[A,B](fa: F[A], b: B)(f: (B, A) => B): B =
    mapAccum(fa, b)((a, b) => (a, f(b, a)))._2

  def zip[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  /*
   * Exercise 12.18
   */
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
    (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {

    def prodApplicative[F[_],G[_]](F: Applicative[F], G: Applicative[G]) =
      new Applicative[({type t[x] = (F[x],G[x])})#t] {
        def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))

        def map2[A,B,C](fa: (F[A], G[A]), fb: (F[B], G[B]))
          (f: (A, B) => C): (F[C], G[C]) =
          (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }

    implicit val pa = prodApplicative(G,H)

    traverse[({type t[x] = (G[x], H[x])})#t, A, B](fa)(a => (f(a), g(a)))
  }

  /*
   * Exercise 12.19
   */
  def compose[G[_]](implicit G: Traverse[G]):
      Traverse[({type t[x] = F[G[x]]})#t] =
    new Traverse[({type t[x] = F[G[x]]})#t] {
      override def traverse[App[_]: Applicative, A, B](fga: F[G[A]])
        (f: A => App[B]): App[F[G[B]]] =
        self.traverse(fga)(ga => G.traverse(ga)(f))
    }
}

/*
 * Exercise 12.20
 */
object Exercise1220 {
  def composeM[F[_], G[_]]
    (implicit F: Monad[F], G: Monad[G], T: Traverse[G]):
      Monad[({type t[x] = F[G[x]]})#t] =
    new Monad[({type t[x] = F[G[x]]})#t] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(fga) { ga => 
          val gfgb: G[F[G[B]]] = G.map(ga)(f)
          val fggb: F[G[G[B]]] = T.sequence(gfgb)
          val fgb:  F[G[B]]    = F.map(fggb)(G.join(_))
          fgb
        }
    }
}


object TraverseInstances {
  /*
   * Exercise 12.13
   */
  val traversableOption = new Traverse[Option] {
    override def sequence[G[_], A](oa: Option[G[A]])
      (implicit app: Applicative[G]): G[Option[A]] = oa match {
      case None     => app.unit(None)
      case Some(ga) => app.map(ga)(Some(_))
    }
  }

  val traversableList = new Traverse[List] {
    override def sequence[G[_], A](as: List[G[A]])
      (implicit app: Applicative[G]): G[List[A]] =
      as.foldRight(app.unit(List(): List[A])) {(ga, gla) =>
        app.map2(ga, gla)((_ :: _))
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val traversableTree = new Traverse[Tree] {
    override def sequence[G[_], A](tree: Tree[G[A]])
      (implicit app: Applicative[G]): G[Tree[A]] = tree match {
      case Tree(h, ltg) =>
        app.map2(h,
          traversableList.sequence(ltg.map(sequence(_))))(Tree(_, _))
    }
  }
}


/*
 * Turning a Monoid into an Applicative
 */

object MonoidToApplicative {
  import fpis.chapter10.Monoid
  import scala.language.implicitConversions

  /*
   * we had from traversable:
   */
  def traverse[F[_], G[_]: Applicative, A, B](fa: F[A])
    (f: A => G[B]): G[F[B]] = ???

  /*
   * if G[X] = Y for all X, this reminds of
   */
  trait MMonoid[_]
  def foldMap[F[_], A, Y](fa: F[A])(f: A => Y)(mb: MMonoid[Y]): Y = ???

  /*
   * thus we try to trick with
   */
  type Const[M,B] = M

  /*
   * what this does: for every Monoid[M], it creates an instance
   * of Applicative. The peculiar thing about this Applicative is that
   * its map2() doesn't use the passed f: (A,B) => B but rather combines
   * the values using Monoid[M].op()
   *
   * this trick, however, doesn't implythat every Monoid is
   * an Applicative, but rather that we construct an Applicative for
   * every Monoid
   */
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M,x]})#f] {
      def unit[A](a: => A): M = M.zero

      def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M =
        M.op(m1, m2)
    }

  /*
   * Exercise 12.15 I cheated here and googled it up
   * if some types occure in the contravariant position, that would
   * fail to be a Functor
   */
}
