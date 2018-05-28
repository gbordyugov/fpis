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

  import Stream._
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
    
    def exists(p: A => Boolean): Boolean = this match {
      case Empty => false
      case Cons(h, t) => p(h()) || t().exists(p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

    def exists_(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)


    /*
     * Exercise 5.4
     */

    def forAll(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) && b)


    /*
     * Exercise 5.5
     */

    def takeWhile_(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A]) {(a, l) =>
        if (p(a))
          cons(a, l)
        else
          Empty
      }


    /*
     * Exercise 5.6
     */

    def headOption1ViaFold: Option[A] =
      foldRight(None: Option[A])((a, oa) => oa match {
          case None => Some(a)
          case _    => None
        }
      )

    // shorter version
    def headOption1ViaFoldBetter: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))


    /*
     * Exercise 5.7
     */

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a, sb) => cons(f(a), sb))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A]){ (a, sa) =>
        if (p(a))
          cons(a, sa)
        else
          sa
      }

    // I accept that I cheated here, especially on the
    // co-/contravariance theme
    // we accept streams of supertypes here
    // append appends to the tail
    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty: Stream[B])((a, as) => f(a).append(as))
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


    /*
     * Exercise 5.8
     */

    def constant[A](a: A): Stream[A] = {
      lazy val s: Stream[A] = cons(a, s)
      s
    }


    /*
     * Exercise 5.9
     */
    
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))


    /*
     * Exercise 5.10
     */

    def fibs: Stream[Int] = {
      lazy val s: Stream[(Int, Int)] =
        cons((0, 1), s.map(a => (a._1 + a._2, a._1)))
      s.map(_._1)
    }
  }
}
