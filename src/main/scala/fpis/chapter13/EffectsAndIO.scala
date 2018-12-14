package fpis.chapter13

import scala.language.higherKinds

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
       *
       * what is happening here is
       *    FlatMap(FlatMap(y, g), f) =
       * (* by the flatMap association law *)
       *  = y.flatMap(g).flatMap(f) =
       *  = y.flatMap(a => g(a).flatMap(f))
       */
      case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
    }
  }

  def badRun[A](t: TailRec[A]): A = t match {
    case Return(x)  => x
    case Suspend(f) => f()
    case FlatMap(x, f) => badRun(f(badRun(x)))
  }

  @annotation.tailrec
  def goodRun[A](t: TailRec[A]): A = t match {
    case Return(x)  => x
    case Suspend(f) => f()
    case FlatMap(x, f) => x match {
      case Return(y) => goodRun(f(y))
      case Suspend(s) => goodRun(f(s()))
      case FlatMap(y, g) => goodRun(y.flatMap(a => g(a).flatMap(f)))
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

object Free {
  /*
   * Exercise 13.1
   */
  sealed trait Free[F[_],A] {
    def flatMap[B](f: A=>Free[F,B]): Free[F,B] =
      FlatMap(this, f)

    def map[B](f: A=>B): Free[F,B] =
      flatMap(x => Return(f(x)))
  }

  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
  case class FlatMap[F[_],A,B](s: Free[F,A],
    f: A => Free[F,B]) extends Free[F,B]

  def freeMonad[F[_]]: Monad[({type t[x] = Free[F,x]})#t] =
    new Monad[({type t[x] = Free[F,x]})#t] {
      def unit[A](a: => A): Free[F,A] = Return[F,A](a)
      def flatMap[A,B](fa: Free[F,A])
        (f: A=>Free[F,B]): Free[F,B] =
        fa.flatMap(f)
    }

  /*
   * Exercise 13.2
   */
  type TailRec[A] = Free[Function0,A]

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(y, g) => runTrampoline(y.flatMap(a => g(a).flatMap(f)))
    }
  }

  /*
   * Exercise 13.3
   */
  @annotation.tailrec
  def step[F[_],A](a: Free[F,A]): Free[F,A] =
    a match {
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => a
    }

  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] =
    a match {
      case Return(a)     => F.unit(a)
      case Suspend(fa)   => fa
      case FlatMap(x, f) => x match {
        case Suspend(r) => F.flatMap(r)(a => run(f(a)))
        case _ => ???
      }
    }

  sealed trait Console[A]
  case object ReadLine extends Console[Option[String]]
  case class PrintLine(line: String) extends Console[Unit]

  object Console {
    type ConsoleIO[A] = Free[Console,A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  trait Translate[F[_],G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_],G[_]] = Translate[F,G]

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a match {
      case ReadLine => { () =>
        val x: Option[String] = try Some(readLine())
          catch { case e: Exception => Option.empty[String] }
        x
      }
      case PrintLine(line) => () => println(line)
    }
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a match {
      case ReadLine => {
        val x: Option[String] =
          try Some(readLine())
          catch { case e: Exception => Option.empty[String] }
        Par.lazyUnit(x)
      }
      case PrintLine(line) => Par.lazyUnit(println(line))
    }
  }

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F~>G)
    (implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])
      (f: A => Function0[B]): Function0[B] =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
      Par.fork { Par.flatMap(a)(f) }
  }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)

  /*
   * Exercise 13.4
   */
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F~>G): Free[G,A] = {
    type T[A] = Free[G,A]
    val t = new (F ~> T) {
      def apply[A](a: F[A]): T[A] = Suspend(fg(a))
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A = {
    val translator = new (Console ~> Function0) {
      def apply[A](a: Console[A]) = a match {
        case ReadLine => { () =>
          val x: Option[String] = try Some(readLine())
          catch { case e: Exception => Option.empty[String] }
          x
        }
        case PrintLine(line) => () => println(line)
      }
    }
    runTrampoline(translate(a)(translator))
  }
}

/*
 * Exercise 13.5
 */
object Exercise1305 {
  import java.nio._
  import java.nio.channels.AsynchronousFileChannel

  import java.util.concurrent.ExecutorService

  import Free._

  trait Source {
    def readBytes(numBytes: Int,
      callback: Either[Throwable,Array[Byte]] => Unit): Unit
  }

  trait Future[+A] {
    def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  /*
   * this one expects a map for a callback (A => Unit) to Unit
   * why?
   */
  def async[A](run: (A => Unit) => Unit): Par[A] =
    es => new Future[A] {
      def apply(k: A => Unit) = run(k)
    }

  /*
   * why not like this?
   */
  def asyncPrime[A](run: A => Unit): Par[A] =
    es => new Future[A] {
      def apply(k: A => Unit) = a => run(a)
    }

  def nonblockingRead(source: Source,
    numBytes: Int): Par[Either[Throwable,Array[Byte]]] =
    async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      source.readBytes(numBytes, cb)
    }

  def readPar(source: Source,
    numBytes: Int): Free[Par,Either[Throwable,Array[Byte]]] =
    Suspend(nonblockingRead(source, numBytes))

  def read(file: AsynchronousFileChannel,
    fromPosition: Long,
    numBytes: Int): Par[Either[Throwable, Array[Byte]]] = ???
}
