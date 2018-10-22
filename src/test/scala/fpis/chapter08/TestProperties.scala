package fpis.chapter08

import org.scalatest._

import java.util.concurrent.{ExecutorService, Executors}

import fpis.chapter06.{State, RNG}
import fpis.chapter06.RNG.nonNegativeInt

import fpis.chapter05.Stream

import fpis.chapter07.Par
import fpis.chapter07.Par.Par


class PropertyTest extends FlatSpec {
  import Prop._

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def testMax(): Unit = {
    import Gen.listOf1
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)
  }


  /*
   * Exercise 8.14
   */

  def testSorted(): Unit = {
    import Gen.listOf1
    val smallInt = Gen.choose(-100, 100)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.isEmpty || sorted.tail.isEmpty ||
        ! sorted.zip(sorted.tail).exists { case (a, b) => a > b }
    }
    run(maxProp)
  }


  /*
   * Testing parallel library
   */

  def testPar(): Unit = {
    val ES: ExecutorService = Executors.newCachedThreadPool

    val p1 = forAll(Gen.unit(Par.unit(1)))(i =>
        Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

    val p2 = check {
      val p1 = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p1(ES).get == p2(ES).get
    }

    val p3 = check {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    }

    run(p1)
    run(p2)
    run(p3)
  }

  def moreTestPar(): Unit = {
    import Gen.{weighted, unit, choose}
		val S = weighted(
			choose(1,4).map(Executors.newFixedThreadPool) -> .75,
      unit(Executors.newCachedThreadPool)           -> .25)

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
      // object ** is defined at the end of the file
      forAll(S ** g) { case s ** a => f(a)(s).get }
    }

    def checkPar(p: Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)

    val p4 = checkPar {
      equal (
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
        )
    }
    run(p4)

    val pint = Gen.choose(0, 10).map(Par.unit(_))
    val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    run(p5)

    /*
     * Exercise 8.16
     */
    def parInt(i: Int) = {
      val lstPar = Par.map(Par.unit(i))(List.fill(10)(_))
      Par.fork(Par.map(lstPar)(lst => lst.foldLeft(0)(_ + _)))
    }
    val pint816: Gen[Par[Int]] = Gen.choose(0, 10).map(parInt)

    /*
     * Exercise 8.17
     */
    val pint817 = Gen.choose(0, 10).map(Par.unit(_))
    val p6 = forAllPar(pint)(n => equal(Par.fork(n), n))
    run(p6)
  }
}
