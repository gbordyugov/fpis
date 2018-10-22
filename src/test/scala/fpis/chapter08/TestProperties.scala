package fpis.chapter08

import org.scalatest._

import java.util.concurrent.{ExecutorService, Executors}

import fpis.chapter06.{State, RNG}
import fpis.chapter06.RNG.nonNegativeInt

import fpis.chapter05.Stream

import fpis.chapter07.Par
import fpis.chapter07.Par.Par


class PropertyTest extends FlatSpec {
  import Prop.{run => propRun, _}

  "max of a list" should "be the largest element" in {
    import Gen.listOf1
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    propRun(maxProp)
  }


  /*
   * Exercise 8.14
   */

  "sorted list" should "have all elements in order" in {
    import Gen.listOf1
    val smallInt = Gen.choose(-100, 100)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.isEmpty || sorted.tail.isEmpty ||
      ! sorted.zip(sorted.tail).exists { case (a, b) => a > b }
    }
    propRun(maxProp)
  }


  /*
   * Testing parallel library
   */

  "parallel things" should "work as expected" in {
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

    propRun(p1)
    propRun(p2)
    propRun(p3)
  }

  "more parallel things" should "work as expected" in {
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
    propRun(p4)

    val pint = Gen.choose(0, 10).map(Par.unit(_))
    val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    propRun(p5)

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
    propRun(p6)
  }
}
