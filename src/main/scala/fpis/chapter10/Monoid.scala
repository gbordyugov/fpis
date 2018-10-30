package fpis.chapter10

import scala.language.higherKinds

import fpis.chapter05.Stream
import fpis.chapter07.Par
import Par.Par

trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

object Chapter10 {
  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(x: List[A], y: List[A]) = x ++ y
    def zero = Nil
  }


  /*
   * Exercise 10.1
   */

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x | y
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x & y
    def zero = true
  }


  /*
   * Exercise 10.2
   */

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    // this would work too
    // def op(x: Option[A], y: Option[A]) = y orElse x
    def zero = None
  }


  /*
   * Exercise 10.3
   */

  def endoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = x => f(g(x))
    def zero = x => x
  }


  /*
   * for Exercise 10.4 see TestMonoid.scala
   */

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /*
   * Exercise 10.5
   */

  def foldMap[X, Y] (xs: List[X], m: Monoid[Y])(f: X => Y): Y =
    concatenate(xs.map(f), m)


  /*
   * Exercise 10.6
   */

  def foldLeft[A, B](a: A)(bs: List[B])(f: (A, B) => A): A = {
    val funcs: List[A => A] = bs.map(b => (a: A) => f(a, b))
    concatenate(funcs, endoMonoid[A])(a)
  }

  def foldRight[A, B](b: B)(as: List[A])(f: (A, B) => B): B = {
    val funcs: List[B => B] = as.map(a => (b: B) => f(a, b))
    /*
     * this should be concatenateRight, but I think it's quite straight-forward
     */
    concatenate(funcs, endoMonoid[B])(b)
  }

  /*
   * Exercise 10.7
   */

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length < 1)
      m.zero
    else if (v.length < 2)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length/2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  /*
   * Exercise 10.8
   */

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a: Par[A], b: Par[A]): Par[A] = Par.map2(a, b)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val bs: Par[List[B]] = Par.parMap(as.toList)(f)
    Par.flatMap(bs) { bs =>
      foldMapV(bs.toIndexedSeq, par(m))(b => Par.lazyUnit(b))
    }
  }

  /*
   * Exercise 10.9
   */

  def isOrdered(as: IndexedSeq[Int]): Boolean =
    if (as.length <= 1)
      true
    else {
      def pairs: IndexedSeq[(Int, Int)] = as.zip(as.tail).map {
        case (a: Int, b: Int) => (a, b)
      }
      foldMap(pairs.toList, booleanAnd) {
        case (a, b) => a <= b
      }
    }

  /*
   * infrastructure for parallel word counting
   */

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /*
   * Exercise 10.10
   */

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero: WC = Stub("")
    def op(x: WC, y: WC): WC = (x, y) match {
      case (Stub(a), Stub(b)) => Stub(a+b)
      case (Part(l, w, r), Stub(a)) => Part(l, w, r+a)
      case (Stub(a), Part(l, w, r)) => Part(a+l, w, r)
      case (Part(l1, w1, ""), Part("", w2, r2)) => Part(l1, w1 + w2, r2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + 1, r2)
    }
  }

  /*
   * Exercise 10.11
   */
  def countWords(s: String): Int = {
    val chars: List[Char] = s.toList

    val folded: WC = foldMap(chars, wcMonoid) {
      case ' ' => Part("", 0, "")
      case c   => Stub(c.toString)
    }

    folded match {
      case Stub("") => 0
      case Stub(_) => 1
      case Part("", n, "") => n
      case Part(_,  n, "") => n + 1
      case Part("", n, _ ) => n + 1
      case Part(_,  n, _ ) => n + 2
    }
  }


  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }


  /*
   * Exercise 10.12
   */
  val listFoldable = new Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  val streamFoldable = new Foldable[Stream] {
    def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
      as.foldRight(z)( (a, b) => f(a, b))

    def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }


  /*
   * Exercise 10.13
   */
  sealed trait Tree[+A] {
    def map[B](f: A=>B): Tree[B] = this match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(l.map(f), l.map(f))
    }
  }

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val treeFoldable = new Foldable[Tree] {
    def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => ???
    }
    def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => ???
    }
    def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as.map(f))(mb.zero)(mb.op)
  }
}
