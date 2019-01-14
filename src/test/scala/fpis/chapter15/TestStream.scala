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

  /*
   * Exercise 15.2
   */
  "count" should "work as expected" in {
    assert(count(Stream(4, 3, 2, 1)).toList === List(1, 2, 3, 4))
  }
}
