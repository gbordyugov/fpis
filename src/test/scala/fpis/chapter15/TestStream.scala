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
    assert(sum(Stream(1, 2, 3, 4)).head === 10)
  }
}
