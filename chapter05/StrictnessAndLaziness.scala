object StrictnessAndLazyness {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond)
      onTrue()
    else
      onFalse()

  def if2_[A](cond: Boolean, onTrue: => A, onFalse: => A):  A =
    if (cond)
      onTrue
    else
      onFalse

  def maybeTwice(b: Boolean, i: => Int) =
    if (b)
      i + i
    else
      0

  def maybeTwice2(b: Boolean, i: => Int) = {
    /*
     * `lazy` keword delays the evaluation of the rhs of that lazy
     * declaration and caches the result of the evaluation
     */
    lazy val j = i
    if (b)
      j + j
    else
      0
  }

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h()) // explicit forcing
    }


    /*
     * Exercise 5.1
     */

    def toListBad: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: (t().toListBad)
    }

    def toList: List[A] = {
      @annotation.tailrec
      def go[A](as: Stream[A], acc: List[A]): List[A] = as match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
      go(this, List.empty).reverse
    }


    /*
     * Exercise 5.2
     */

     def take(n: Long): Stream[A] = n match {
       case 0          => Empty
       case m if m > 0 => this match {
         case Empty      => ???
         case Cons(h, t) => Cons(h, () => t().take(m - 1))
       }
     }

     def drop(n: Long): Stream[A] = this match {
       case Empty => Empty
       case Cons(h, t) if n == 0 => this
       case Cons(h, t)           => t().drop(n-1)
     }


     /*
      * Exercise 5.3
      */

     def takeWhile(p: A => Boolean): Stream[A] = this match {
       case Empty => Empty
       case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
       case _ => Empty
     }
  }

  case object Empty extends Stream[Nothing]
  // the explicit thunks are a technical necessity here
  case class Cons[+A](h: () => A, t: () => Stream[A])
    extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head, apply(as.tail: _*))
    }
}
