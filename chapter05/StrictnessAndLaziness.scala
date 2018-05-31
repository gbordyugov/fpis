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


    /*
     * Exercise 5.13
     */

    def mapByUnfold[B](f: A => B): Stream[B] =
      unfold(this) { _ match {
          case Empty      => None
          case Cons(h, t) => Some((f(h()), t()))
        }
      }

    def takeByUnfold(i: Int): Stream[A] =
      unfold((i, this)) {
        case (j, Cons(h, t)) if j > 0 => Some((h(), (j-1, t())))
        case _                        => None
      }

    def takeWhileByUnfold(p: A => Boolean): Stream[A] =
      unfold(this) { _ match {
          case Cons(h, t) if p(h()) => Some((h(), t()))
          case _                    => None
        }
      }

    def zipWithByUnfold[B, C](that: Stream[B])
      (f: (A, B) => C): Stream[C] =
      unfold((this, that)) { _ match {
          case (Cons(a, b), Cons(x, y)) =>
            Some((f(a(), x()), (b(), y())))
          case _                        => None
        }
      }

    def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, that)) { _ match  {
          case (Cons(a, b), Cons(x, y)) =>
            Some(((Some(a()), Some(x())), (b(), y())))
          case (Cons(a, b), Empty)      =>
            Some(((Some(a()), None), (b(), empty)))
          case (Empty, Cons(x, y))      =>
            Some(((None, Some(x())), (empty, y())))
          case _                        => None
        }
      }


    /*
     * Exercise 5.14
     */

    def startsWith[A](that: Stream[A]): Boolean = (this, that) match {
      case (_, Empty)                             => true
      case (Cons(a, b), Cons(x, y)) if a() == (x) => b().startsWith(y())
      case _                                      => false
    }

    def length: Long = this.foldRight(0)((_, c) => c + 1)

    def startsWith1[A](that: Stream[A]): Boolean =
      this.zipWithByUnfold(that)(_ == _)
      .takeWhileByUnfold(x => x).length == that.length
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


    /*
     * Exercise 5.11
     */

    def badUnfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      lazy val as: Stream[Option[(A, S)]] =
        cons(f(z), as.map(x => x.map(_._2).flatMap(f)))
      lazy val bs = as.takeWhile { a =>
        a match {
          case None => false
          case _    => true
        }
      }
      bs.map(oas => oas.get._1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None         => empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }


    /*
     * Exercise 5.12
     */

    def fibsByUnfold: Stream[Int] =
      unfold((0, 1)){ s => Some(s._1, (s._1 + s._2, s._1)) }

    def fromByUnfold(i: Int): Stream[Int] =
      unfold(i)(s => Some(s, s+1))

    def constantByUnfold[A](a: A): Stream[A] =
      unfold(a)(s => Some(a, a))

    def onesByUnfold: Stream[Int] = constantByUnfold(1: Int)
  }
}
