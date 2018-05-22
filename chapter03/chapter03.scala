object Chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class  Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = 
      if (as.isEmpty)
        Nil
      else
        Cons(as.head, apply(as.tail: _*))

    def sum(xs: List[Int]): Int = xs match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }


    /*
     * Exercise 3.1
     */

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }


    /*
     * Exercise 3.2
     */

    def tail[A](l: List[A]): List[A] = l match {
      case Cons(h, t) => t
      case _          => ???
    }


    /*
     * Exercise 3.3
     */

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Cons(h_, t) => Cons(h, t)
      case _           => ???
    }


    /*
     * Exercise 3.4
     */

    def drop[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case n => drop(tail(l), n - 1)
    }


    /*
     * Exercise 3.5
     */

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
    }

    def append[A](a: List[A], b: List[A]): List[A] = a match {
      case Nil         => b
      case Cons (h, t) => Cons(h, append(t, b))
    }


    /*
     * Exercise 3.6 (first solution)
     *
     * this one is three-pass solution, but is tail-recursive
     */

    def init[A](l: List[A]): List[A] = {
      @annotation.tailrec
      def rev(a: List[A], acc: List[A] = Nil): List[A] = a match {
        case Nil => acc
        case Cons(h, t) => rev(t, Cons(h, acc))
      }
      rev(tail(rev(l)))
    }


    /*
     * Exercise 3.6 (second solution)
     *
     * this one is not tail-recursive, but is a one-pass solution
     */

    def init_[A](l: List[A]): List[A] = l match {
      case Nil                   => ???
      case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
      case Cons(h, tail)         => Cons(h, init(tail))
    }


    def dropWhile_[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile_(t)(f)
      case _                  => l
    }


    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(head, tail) => f(head, foldRight(tail, z)(f))
      }

    def sum2    (as: List[Int])    = foldRight(as,   0)(_ + _)
    def product2(as: List[Double]) = foldRight(as, 1.0)(_ * _)


    /*
     * Exercise 3.7
     */

    def productShortcut(as: List[Double]): Double = as match {
      case Nil              => 1.0
      case Cons(0.0, tail)  => 0.0
      case Cons(head, tail) => head * productShortcut(tail)
    }
    /*
     * I don't see how to implement this shortcutting using foldRight
     */


    /*
     * Exercise 3.8
     *
     * foldRight will re-build the original list
     */


    /*
     * Exercise 3.9
     */

    def length[A](as: List[A]) = foldRight(as, 0)((a, c) => c + 1)


    /*
     * Exercise 3.10
     */

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }


    /*
     * Exercise 3.11
     */

    def sum3(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
    def product3(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
    def length3[A](as: List[A]): Int = foldLeft(as, 0)((z, a) => z + 1)


    /*
     * Exercise 3.12
     */

    def rev[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((as, a) => Cons(a, as))


    /*
     * Exercise 3.13
     */

    def foldRight_[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      val id: B => B = b => b
      // val g: (B => B, A) => (B => B) = ((g, a) => (b => f(a, g(b))))
      val g: (B => B, A) => (B => B) = ((g, a) => (b => g(f(a, b))))
      val h: B => B = foldLeft(as, id)(g)
      h(z)
    }


    /*
     * Exercise 3.14
     */

    def append_[A](a: List[A], b: List[A]): List[A] = {
      foldLeft(rev(a), b)((bs, a) => Cons(a, bs))
    }

    def append__[A](a: List[A], b: List[A]): List[A] = {
      foldRight(a, b)(Cons(_, _))
    }


    /*
     * Exercise 3.15
     */

    def concatenate[A](l: List[List[A]]): List[A] = {
      // could be implemented through foldRight by foldLeft
      foldRight(l, Nil: List[A])((a, b) => append(a, b))
    }


    /*
     * Exercise 3.16
     */

    def add1(as: List[Int]): List[Int] = as match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head+1, add1(tail))
    }


    /*
     * Exercise 3.17
     */

    def d2s(ds: List[Double]): List[String] = ds match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head.toString, d2s(tail))
    }


    /*
     * Exercise 3.18
     */

    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }


    /*
     * Exercise 3.19
     */

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h, t)         => filter(t)(f)
    }


    /*
     * Exercise 3.20
     */

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      concatenate(map(as)(f))


    /*
     * Exercise 3.21
     */

    def filter_[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)


    /*
     * Exercise 3.22
     */

    def addLists(as: List[Int], bs: List[Int]): List[Int] =
      (as, bs) match {
        case (Cons(a, as), Cons(b, bs)) => Cons(a+b, addLists(as, bs))
        case _ => Nil
      }
  }
}
