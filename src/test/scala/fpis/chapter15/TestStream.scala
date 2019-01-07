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
}
