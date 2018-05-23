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
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
