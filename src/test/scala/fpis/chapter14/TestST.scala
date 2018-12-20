package fpis.chapter14

import org.scalatest._

class TestST extends FlatSpec with Matchers {
  "bla" should "blo" in {
    /*
     * now that we can construct ST[_,_] values, we don't really know
     * how to run them, since ST.run() is protected
     */
    val test: ST[Nothing,(Int, Int)] = for {
      r1 <- STRef[Nothing,Int](1)
      r2 <- STRef[Nothing,Int](1)
      x  <- r1.read
      y  <- r2.read
      _  <- r1.write(y+1)
      _  <- r2.write(x+1)
      a  <- r1.read
      b  <- r2.read
    } yield (a, b)
  }

  "bli" should "blu" in {
    val p = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- STRef[S,Int](1)
        r2 <- STRef[S,Int](1)
        x  <- r1.read
        y  <- r2.read
        _  <- r1.write(y+1)
        _  <- r2.write(x+1)
        a  <- r1.read
        b  <- r2.read
      } yield (a, b)
    }
  }
}
