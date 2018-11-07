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
   */
  def applyViaMap2AndUnit[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def mapViaApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    applyViaMap2AndUnit(unit(f))(fa)

  def map2ViaApplyAndUnit[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val g: F[A => B => C] = unit(a => b => f(a, b))
    val first: F[B => C] = applyViaMap2AndUnit(g)(fa)
    applyViaMap2AndUnit(first)(fb)
  }
}
