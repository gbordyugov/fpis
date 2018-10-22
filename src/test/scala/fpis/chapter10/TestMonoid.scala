package fpis.chapter10

import org.scalatest._

import fpis.chapter08.Prop.{run => propRun, _}
import fpis.chapter08.{Gen, Prop}
import fpis.chapter08.Gen._

import Chapter10._

class TestMonoid extends FlatSpec {

  /*
   * Exercise 10.4
   */

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => m.op(a, m.zero) == a &&
                     m.op(m.zero, a) == a)

  "string monoid" should "satisfy monoid laws" in {
    val strings = stringN(100)
    val monoidProp = monoidLaws(stringMonoid, strings)
    justRun(monoidProp) match {
      case Falsified(_, _) => assert(false)
      case _              => assert(true)
    }
  }
}
