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
}
