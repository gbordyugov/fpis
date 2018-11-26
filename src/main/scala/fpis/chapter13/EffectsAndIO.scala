package fpis.chapter13

import fpis.chapter07.Par
import fpis.chapter07.Par.Par


/*
 * first attempt at IO
 */
object IO0 {
  sealed trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      /*
       * here is a potential stack overflow risk, since it runs self
       * before calling `f` on its result
       */
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
}


object IO1 {

  /*
   * it used to be called IO[A], later in the book it is renamed
   * to TailRec[A]
   */
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] =
      // flatMap(f andThen (Return(_)))
      flatMap(x => Return(f(x)))

  }

  /*
   * The main difference between Return and Suspend is that the latter
   * is lazy in its argument
   * Suspend is similar to Par.lazyUnit()
   */
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]

  /*
   * This constructor holds the state of a flatMap operation, without
   * actually executing it
   */
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B])
      extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)

    def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      a.flatMap(f)

    def apply[A](a: => A): TailRec[A] = Suspend(() => a)
  }

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    /*
     * For FlatMap, run(f(run(x)) would work too, but is not
     * tail-recursive
     */
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      /*
       * Here is where the chaining of flatmaps happens. Please see
       * the bottom of p. 238 for the explanation of how to
       * re-associate the stack fo flatMaps
       */
      case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
    }
  }
}

object Async {
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] =
      a.flatMap(f)
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A,B](sub: Async[A],
    k: A => Async[B]) extends Async[B]

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) =>
      step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)     => Par.unit(a)
    case Suspend(r)    => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _          =>
        sys.error("Impossible; `step` elimintates these cases")
    }
  }
}
