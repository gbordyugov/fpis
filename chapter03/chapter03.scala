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
  }


    /*
     * Exercise 3.10
     */

    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
}