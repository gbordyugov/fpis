package fpis.chapter15

import scala.collection.immutable.Stream

import Process._

import org.scalatest._

class StreamTest extends FlatSpec with Matchers {
  "lift" should "work as expected" in {
    val units = Stream.continually(())
    val ones = lift((_: Unit) => 1)(units)

    assert(ones.head === 1)
  }

  "filter" should "work as expected" in {
    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1, 2, 3, 4)).toList
    assert(evens === List(2, 4))
  }

  "sum" should "work as expected" in {
    assert(sum(Stream(1, 2, 3, 4)).toList === List(1, 3, 6, 10))
  }

  /*
   * Exercise 15.1
   */
  "drop" should "drop stuff" in {
    assert(drop(2)(Stream(1, 2, 3, 4)) === Stream(3, 4))
  }

  "take" should "take stuff" in {
    assert(take(2)(Stream(1, 2, 3, 4)) === Stream(1, 2))
  }

  "takeWhile" should "take while" in {
    assert(takeWhile((_: Int) <= 3)(Stream(1, 2, 3, 4)) === Stream(1, 2, 3))
  }

  "dropWhile" should "take while" in {
    assert(dropWhile((_: Int) <= 3)(Stream(1, 2, 3, 4, 5)) === Stream(4, 5))
  }

  /*
   * Exercise 15.2
   */
  "count" should "work as expected" in {
    assert(count(Stream(4, 3, 2, 1)) === Stream(1, 2, 3, 4))
  }


  /*
   * Exercise 15.3
   */
  "mean" should "calculate the mean correctly" in {
    assert(mean(Stream(1, 2, 3, 4)) === Stream(1, 1.5, 2, 2.5))
  }

  /*
   * Exercise 15.4
   */
  "sumViaLoop" should "work as expected" in {
    assert(sumViaLoop(Stream(1, 2, 3, 4)).toList === List(1, 3, 6, 10))
  }

  "meanViaLoop" should "calculate the mean correctly" in {
    assert(meanViaLoop(Stream(1, 2, 3, 4)) === Stream(1, 1.5, 2, 2.5))
  }

  /*
   * Exercise 15.5
   */
  "pipe" should "compose processes" in {
    val p1: Process[Int,Int] = filter(_ % 2 == 0)
    val p2: Process[Int,Int] = lift(_ + 1)
    val p3: Process[Int,Int] = filter(_ >= 5)
    val p: Process[Int,Int] = p1 |> p2

    assert(p1(Stream(0, 1, 2, 3)) === Stream(0, 2))
    assert(p2(Stream(0, 1, 2, 3)) === Stream(1, 2, 3, 4))

    assert(p(Stream(0, 1, 2, 3, 4, 5, 6)) === Stream(1, 3, 5, 7))
    assert((p |> p3)(Stream(0, 1, 2, 3, 4, 5, 6)) === Stream(5, 7))
  }

  "zip" should "do stuff" in {
    val p: Process[Double,Double] = echo
    val q: Process[Double,Double] = echo

    assert(p.zip(q)(Stream(1, 2, 3)) === Stream((1, 1), (2, 2), (3, 3)))
  }

  /*
   * Exercise 15.7
   */
  "mean157" should "calculate the mean correctly" in {
    assert(mean157(Stream(1, 2, 3, 4)) === Stream(1, 1.5, 2, 2.5))
  }
}
