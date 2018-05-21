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
     */

    def init[A](l: List[A]): List[A] = {
      def rev(a: List[A], acc: List[A] = Nil): List[A] = a match {
        case Nil => acc
        case Cons(h, t) => rev(t, Cons(h, acc))
      }
      rev(tail(rev(l)))
    }
  }
}
