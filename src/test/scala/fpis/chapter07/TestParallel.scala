package fpis.chapter07

import java.util.concurrent.{ExecutorService, Future, TimeUnit,
  Callable, Executors}

import org.scalatest._

class ParallelTest extends FlatSpec with Matchers {
  import Par.{run => parRun, _}

  val es = Executors.newFixedThreadPool(2)
  def run[A](p: Par[A]) = parRun(es)(p).get

  "unit()" should "evaluate correctly" in {
    assert(run(unit(1)) === 1)
  }

  "map2()" should "calculate things correctly" in {
    val a: Par[Int] = unit(1)
    val b: Par[Int] = unit(2)
    assert(run(map2(a, b)(_ + _)) === 3)
  }

  "fork()" should "calculate things correctly" in {
    assert(run(fork(unit(1))) === 1)
  }

  "lazyUnit()" should "evaluate correctly" in {
    assert(run(lazyUnit(1)) === 1)
  }

  "asyncF()" should "compute things correctly" in {
    val addThree: Int => Par[Int] = asyncF(_ + 3)
    assert(run(addThree(5)) === 8)
  }

  val things = (1 to 500).map(Par.lazyUnit(_)).toList
  val thing = Par.sequence(things)

  val anotherThing = Par.map2(Par.lazyUnit(1), Par.lazyUnit(1))(_ + _)

  val p1 = Par.unit(1)
  val p2 = Par.fork(p1)
  val p3 = Par.fork(p2)
  val p4 = Par.fork(p3)

  val lst = Par.unit(List(3, 4, 1, 2))
  val sortedLst = Par.map(lst)(_.sorted)

  def sumSeq(s: Seq[Int]): Par.Par[Int] = {
    val length = s.length
    if (length <= 100)
      Par.lazyUnit(s.foldRight(0)(_ + _))
    else {
      val (l, r) = s.splitAt(length/2)
      Par.map2(sumSeq(l), sumSeq(r))(_ + _)
    }
  }
}
