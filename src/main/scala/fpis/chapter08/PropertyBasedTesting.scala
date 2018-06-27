package fpis.chapter08

import fpis.chapter06.{State, RNG}
import fpis.chapter06.RNG.nonNegativeInt

/*
 * Exercise 8.1
 *
 * sum of the reverse should be equal to the sum of the list
 * sum of N repetitions of a number P should be equal NxP
 * sum of an non-empty list should be equal to the sum of its head
 *   plus the sum of the tail
 * sum of a list should be equal to the sum of the sums of its partitions
 * sum of any permutation of the list should be the same
 */

/*
 * Exercise 8.2
 *
 * maximum should not be smaller than any element of the list
 * maximum of the reversed list should be equal to the maximum of
 *   the list
 * max of any permutation should be the same
 * max of a non-empty list should be max(head, max(tail))
 */

trait Prop {
  def check: Unit
  def &&(p: Prop): Prop
}

/*
 * Exercise 8.3
 */
object Exercise83 {
  trait Prop { outer =>
    def check: Boolean
    def &&(that: Prop): Prop = new Prop {
      def check = outer.check && that.check
    }
  }
}


case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen[B](sample.map(f))
}


object Gen {
  /*
   * Exercise 8.4
   */
  // case class State[S, +A](run: S => (A, S)) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](State(nonNegativeInt _).map{ i =>
      start + i % (stopExclusive - start)})


  /*
   * Exercise 8.5
   */
  def unit[A](a: => A): Gen[A] =
    Gen[A](State.unit(a))

  def boolean: Gen[Boolean] =
    Gen[Boolean](State(nonNegativeInt _).map{ _%2 > 0})

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen[List[A]](State.sequence(List.fill(n)(g.sample)))


  /*
   * Given Gen[Int] generator, produce Gen[(Int, Int)]
   */

  def pair[A](g: Gen[A]): Gen[(A, A)] =
    Gen[(A, A)](for { a <- g.sample; b <- g.sample } yield((a, b)))


  /*
   * Given Gen[A], produce Gen[Option[A]]
   */

  def option[A](g: Gen[A]): Gen[Option[A]] =
    Gen[Option[A]](g.sample.map(Some(_)))


  /*
   * Generating strings
   */

  def char: Gen[Char] =
    choose(97, 123).map(_.asInstanceOf[Char])

  def stringN(n: Int): Gen[String] =
    listOfN(n, char).map(_.mkString)
}
