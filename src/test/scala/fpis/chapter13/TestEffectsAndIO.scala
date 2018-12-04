package fpis.chapter13

import org.scalatest._

class TestIO1 extends FlatSpec with Matchers {
  import IO1.{run => _, _}
  val x: TailRec[Int] = Return(1)

  /*
  val func: TailRec[Int] =
    List.fill(100000)(1).foldLeft(x) { (x, _) =>
      x.flatMap(Return(_))
    }
   */

  /*
   * this causes stack overflow error
   */
  // assert(1 === badRun(func))

  /*
   * this doesn't cause stack overflow error
   */
  // assert(1 === goodRun(func))

  def trivialize[A](a: TailRec[A]): TailRec[A] =
    a.flatMap(Return[A](_))

  @annotation.tailrec
  final def myGoodRun[A](t: TailRec[A]): A = {
    println(s"entering myGoodRun with t=$t")
    t match {
      case Return(x)  => x
      case Suspend(f) => f()
      case FlatMap(x, f) => x match {
        case Return(y) => myGoodRun(f(y))
        case Suspend(s) => myGoodRun(f(s()))
        case FlatMap(y, g) => myGoodRun(y.flatMap(a => g(a).flatMap(f)))
      }
    }
  }

  val y = trivialize(trivialize(x))
  assert(1 === myGoodRun(y))
}

class TestFree extends FlatSpec with Matchers {
  import Free._, Free.Console._

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln
}
