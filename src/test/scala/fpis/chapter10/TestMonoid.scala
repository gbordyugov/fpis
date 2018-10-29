package fpis.chapter10

import org.scalatest._

import fpis.chapter08.Prop.{run => propRun, _}
import fpis.chapter08.{Gen, Prop}
import fpis.chapter08.Gen._

import fpis.chapter07.{Par, Result}

class TestMonoid extends FlatSpec with Matchers {
  import java.util.concurrent.Executors

  val es = Executors.newFixedThreadPool(2)
  def runPar[A](p: Par.Par[A]) = {
    val r: Result[A] = Par.run(es)(p).get
    println(s"fork depth ${r.trace.depth}")
    r.value
  }

  /*
   * Exercise 10.4
   */

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val pairs: Gen[(A, A)] = gen.map2(gen)((_, _))
    val triples: Gen[(A, A, A)] = pairs.map2(gen) {
      case ((a, b), c) => (a, b, c)
    }

    forAll(triples) {case (a, b, c) =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a &&
      m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }
  }

  "string monoid" should "satisfy monoid laws" in {
    import Chapter10.stringMonoid

    val strings = stringN(100)
    val monoidProp = monoidLaws(stringMonoid, strings)
    justRun(monoidProp) match {
      case Falsified(_, _) => assert(false)
      case _               => assert(true)
    }
  }

  /*
   * Exercise 10.5
   */

  "foldMap" should "be able to sum up a list of ints" in {
    import Chapter10.{intAddition, foldMap}
    val ints: List[Int] = List(1, 2, 3, 4, 5)
    assert(foldMap(ints, intAddition)(x => x) === ints.foldLeft(0)(_ + _))
  }

  /*
   * Exercise 10.6
   */

  "new foldLeft" should "be able to sum a list of ints" in {
    import Chapter10.foldLeft
    val ints: List[Int] = (1 to 10).toList
    assert(foldLeft(0)(ints)(_ + _) === ints.foldLeft(0)(_ + _))
  }

  "new foldRight" should "be able to sum a list of ints" in {
    import Chapter10.foldRight
    val ints: List[Int] = (1 to 10).toList
    assert(foldRight(0)(ints)(_ + _) === ints.foldLeft(0)(_ + _))
  }

  /*
   * Exercise 10.7
   */

  "foldMapV" should "be able to sum a list of ints" in {
    import Chapter10.{foldMapV, intAddition}
    val ints: IndexedSeq[Int] = (1 to 10).toIndexedSeq
    assert(foldMapV(ints, intAddition)(x => x) === ints.foldLeft(0)(_ + _))
  }

  /*
   * Exercise 10.8
   */
  "parFoldMap" should "be able to sum a list of ints" in {
    import Chapter10.{parFoldMap, intAddition}

    val ints: IndexedSeq[Int] = (1 to 10).toIndexedSeq
    assert(runPar(parFoldMap(ints, intAddition)(x => x))
      === ints.foldLeft(0)(_ + _))
  }


  /*
   * Exercise 10.9
   */
  "isOrdered()" should "recognize ordered lists" in {
    import Chapter10.{booleanAnd, isOrdered}

    isOrdered(IndexedSeq())  should be (true)
    isOrdered(IndexedSeq(1)) should be (true)
    isOrdered(IndexedSeq(1, 2, 3)) should be (true)
    isOrdered(IndexedSeq(1, 1, 1)) should be (true)
    isOrdered(IndexedSeq(3, 2, 1)) should be (false)
    isOrdered(IndexedSeq(3, 1, 2)) should be (false)
    isOrdered(IndexedSeq(3, 3, 2)) should be (false)
  }


  /*
   * Exercise 10.10
   */

  "word counting monoid" should "behave sensible" in {
    import Chapter10.{WC, Part, Stub, wcMonoid}
    import wcMonoid._

    zero should be (Stub(""))

    op(Stub("a"), zero) should be (Stub("a")) 
    op(zero, Stub("a")) should be (Stub("a"))

    op(Stub("a"), Stub("b")) should be (Stub("ab"))
    op(Part("a", 1, "b"), Stub("c")) should be (Part("a", 1, "bc"))
    op(Stub("c"), Part("a", 1, "b")) should be (Part("ca", 1, "b"))
    op(Part("a", 1, "b"), Part("x", 2, "y")) should be (Part("a", 4, "y"))
  }

  /*
   * Exercise 10.11
   */

  "countWords()" should "count words correctly" in {
    import Chapter10.countWords

    countWords("") should be (0)
    countWords(" ") should be (0)
    countWords("  ") should be (0)
    countWords("   ") should be (0)
    countWords("    ") should be (0)
    countWords("bla") should be (1)
    countWords("bla bla") should be (2)
    countWords("bla bla bla") should be (3)
    countWords("bla bla bla bla") should be (4)
    countWords("bla   bla     bla   bla") should be (4)
  }
}
