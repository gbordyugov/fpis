package fpis.chapter14

import org.scalatest._

class TestST extends FlatSpec with Matchers {
  "bla" should "blo" in {
    val test = for {
      r1 <- STRef[Nothing,Int](1)
      r2 <- STRef[Nothing,Int](1)
      a  <- r1.read
      b  <- r2.read
    } yield (a, b)
  }
}
