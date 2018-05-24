import math.pow

object ChapterOption {
  sealed trait Option[+A] {
    /*
     * Exercise 4.1
     */
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None    => default
      case Some(x) => x
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None    => ob
      case Some(x) => Some(x)
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None            => None
      case Some(x) if f(x) => Some(x)
      case _               => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  /*
   * Exercise 4.2
   */

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m =>
      mean(xs.map(x => pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f


  /*
   * Exercise 4.3
   */

  def map2[A, B, C](oa: Option[A],
                    ob: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      a <- oa
      b <- ob
      } yield f(a, b)
  }

  def map2_[A, B, C](oa: Option[A],
                     ob: Option[B])(f: (A, B) => C): Option[C] =
    (oa, ob) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None
    }


  /*
   * Exercise 4.4
   */

  // my own solution
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil             => Some(Nil)
    case Some(a) :: tail => sequence(tail) match {
      case Some(s) => Some(a :: s)
      case None    => None
    }
    case None :: _       => None
  }

  def sequence_[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) =>
      map2(x, y)(_ :: _)
    )

  def sequence__[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(x => sequence__(t).map(s => x::s))
  }
}
