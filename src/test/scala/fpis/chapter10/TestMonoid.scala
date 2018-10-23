package fpis.chapter10

import org.scalatest._

import fpis.chapter08.Prop.{run => propRun, _}
import fpis.chapter08.{Gen, Prop}
import fpis.chapter08.Gen._

class TestMonoid extends FlatSpec {

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
    val intsMonoid = intAddition
    assert(foldMap(ints, intAddition)(x => x) == ints.foldLeft(0)(_ + _))
  }
}
