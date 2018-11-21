package fpis.chapter13

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    def run = f(self.run).run
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }

  def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa.flatMap(f)

  def apply[A](a: => A): IO[A] = unit(a)

  /*
   * some primitives
   */
  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }
}

object SimpleGame {
  import IO._

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score)
      Some(p1)
    else if (p2.score > p1.score)
      Some(p2)
    else
      None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))
}

object Converter {
  import IO._

  def fToC(f: Double): Double =
    (f - 32.0) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in F: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fToC(d).toString)
  } yield()
}
