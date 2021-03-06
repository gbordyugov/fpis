package fpis.chapter07

import java.util.concurrent.{ExecutorService, Future, TimeUnit,
  Callable, Executors}

import org.scalatest._

class ParallelTest extends FlatSpec with Matchers {
  import Par.{run => parRun, _}

  val es = Executors.newFixedThreadPool(4)
  def run[A](p: Par[A]) = {
    val result: Result[A] = parRun(es)(p).get
    val value: A = result.value
    val forkTrace: Trace = result.trace
    (result.value, forkTrace.depth)
  }

  "unit()" should "evaluate correctly" in {
    assert(run(unit(1)) === (1, 0))
  }

  "map2()" should "calculate things correctly" in {
    val a: Par[Int] = unit(1)
    val b: Par[Int] = unit(2)
    assert(run(map2(a, b)(_ + _)) === (3, 0))
  }

  it should "calculate forked Pars correctly" in {
    val a: Par[Int] = unit(1)
    val b: Par[Int] = unit(2)
    assert(run(map2(fork(a), fork(b))(_ + _)) === (3, 1))
  }

  "fork()" should "calculate things correctly" in {
    assert(run(fork(unit(1))) === (1, 1))
  }

  "lazyUnit()" should "evaluate correctly" in {
    assert(run(lazyUnit(1)) === (1, 1))
  }

  "asyncF()" should "compute things correctly" in {
    val addThree: Int => Par[Int] = asyncF(_ + 3)
    assert(run(addThree(5)) === (8, 1))
  }

  "sequence()" should "do right things" in {
    val lst: List[Int] = (1 to 500).toList
    val things = lst.map(lazyUnit(_)).toList
    val thing = Par.sequence(things)
    assert(run(thing) === (lst, 1))
  }

  "four forks" should "produce a fork depth of three" in {
    val p1 = Par.unit(1)
    val p2 = Par.fork(p1)
    val p3 = Par.fork(p2)
    val p4 = Par.fork(p3)
    assert(run(p4) === (1, 3))
  }

  "map()" should "be able to sort a list" in {
    val lst = Par.lazyUnit(List(3, 4, 5, 1, 2))
    val sortedLst = Par.map(lst)(_.sorted)
    assert(run(sortedLst) === (List(1, 2, 3, 4, 5), 1))
  }

  "parallel summation" should "work as expected" in {
    def sumSeq(s: Seq[Int]): Par[Int] = {
      val length = s.length
      if (length <= 100)
        Par.lazyUnit(s.foldRight(0)(_ + _))
      else {
        val (l, r) = s.splitAt(length/2)
        Par.map2(sumSeq(l), sumSeq(r))(_ + _)
      }
    }
    val lst: List[Int] = (1 to 500).toList
    assert(run(sumSeq(lst)) === (lst.foldLeft(0)(_+_), 1))
  }

  "parMap()" should "map ints to strings correctly" in {
    val ints: List[Int] = (1 to 500).toList
    val strs: Par[List[String]] = parMap(ints)(_.toString)
    assert(run(strs) === (ints.map(_.toString), 2))
  }
}
