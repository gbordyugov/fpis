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
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
