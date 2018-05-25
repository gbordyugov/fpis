object ChapterEither {
  sealed trait Either[+E, +A] {


    /*
     * Exercise 4.6
     */

    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(b)  => Left(b)
    }

    def flatMap[EE >:E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => f(a)
        case Left(b)  => Left(b)
      }


  }

  case class Left[+E] (value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
}
