package fpis.chapter13

import org.scalatest._

class TestIO1 extends FlatSpec with Matchers {
  import IO1.{run => _, _}
  val x: TailRec[Int] = Return(1)
  val func: TailRec[Int] =
    List.fill(100000)(1).foldLeft(x) { (x, _) =>
      x.flatMap(Return(_))
    }

  /*
   * this causes stack overflow error
   */
  // assert(1 === badRun(func))

  /*
   * this doesn't cause stack overflow error
   */
  assert(1 === goodRun(func))
}
