package fpis.chapter13.TwoConstructors

import scala.language.higherKinds

import fpis.chapter13.{Monad, Functor}

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B])
    (implicit F: Functor[F]): Free[F,B] = this match {
    case Return(a)  => f(a)
    case Suspend(fa) => Suspend(F.map(fa)(ffa => ffa.flatMap(f)))
  }

  def map[B](f: A => B)(implicit F: Functor[F]): Free[F,B] =
    flatMap(x => Return(f(x)))
}

case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](f: F[Free[F,A]]) extends Free[F,A]

object Free {
  def freeMonad[F[_]](implicit F: Functor[F]):
    Monad[({type t[x] = Free[F,x]})#t] =
    new Monad[({type t[x] = Free[F,x]})#t] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A,B](a: Free[F,A])(f: A => Free[F,B]): Free[F,B] =
        a.flatMap(f)
    }

  def run[F[_],A](f: Free[F,A])(implicit F: Monad[F]): F[A] = f match {
    case Return(a)   => F.unit(a)
    case Suspend(fa) => F.join(F.map(fa)(a => run(a)))
  }

  trait Translate[F[_],G[_]] {
    def apply[A](a: F[A]): G[A]
  }

  type ~>[F[_],G[_]] = Translate[F,G]

  def runFree[F[_],G[_],A](f: Free[F,A])(t: F~>G)
    (implicit F: Functor[F], G: Monad[G]): G[A] = f match {
    case Return(a)   => G.unit(a)
    case Suspend(fa) => G.join(t(F.map(fa)(a => runFree(a)(t))))
  }

  def translate[F[_],G[_],A](f: Free[F,A])(t: F~>G)
      (implicit F: Functor[F], G: Functor[G]): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val int = new (F~>FreeG) {
      def apply[A](fa: F[A]): FreeG[A] =
        Suspend(G.map(t(fa))(Return(_)))
    }
    runFree[F,FreeG,A](f)(int)(F, freeMonad[G])
  }
}
