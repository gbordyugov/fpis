package fpis.chapter08

import fpis.chapter06.{State, RNG}
import fpis.chapter06.RNG.nonNegativeInt

import fpis.chapter05.Stream

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

/*
trait Prop {
  def check: Unit
  def &&(p: Prop): Prop
}
*/


object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  type MaxSize = Int
  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    /*
     * Exercise 8.9
     */
    def &&(that: Prop): Prop =
      Prop {
        (ms, n, rng) => run(ms, n, rng) match {
          case Passed => that.run(ms, n, rng)
          case result => result
        }
      }

    def ||(that: Prop): Prop =
      Prop {
        (ms, n, rng) => run(ms, n, rng) match {
          case Passed => Passed
          // this drops the information from the first run
          // potential bug
          case result => that.run(ms, n, rng)
        }
      }
  }


  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a))
          Passed
        else
          Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize: Int = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
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


case class SGen[+A](forSize: Int => Gen[A]) {
  /*
   * Exercise 8.11
   */
  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g: Int => Gen[B] =
      n => forSize(n).flatMap(a => f(a)(n))
    SGen(g)
  }
}


case class Gen[+A](sample: State[RNG, A]) {
  import Gen._


  /*
   * Exercise 8.10
   */
  def unsized: SGen[A] =
    SGen[A](_ => this)

  def map[B](f: A => B): Gen[B] =
    Gen[B](sample.map(f))


  /*
   * Exercise 8.6
   */

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen[B](sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap { s =>
      (1 to s).foldRight (unit(List(): List[A])) { (i, list) => for {
            h <- this
            t <- list
          } yield h :: t
        }
    }
  }

}


object Gen {
  /*
   * Exercise 8.4
   */
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


  /*
   * Exercise 8.7
   */

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)


  /*
   * Exercise 8.8
   */

  def positiveInt: Gen[Int] =
    Gen[Int](State(nonNegativeInt _))

  def double: Gen[Double] =
    positiveInt.map(_ / Int.MaxValue)

  def weighted[A](p1: (Gen[A], Double), p2: (Gen[A], Double)): Gen[A] = {
    val (g1, w1) = p1
    val (g2, w2) = p2
    val threshold = w1/(w1 + w2)
    double.flatMap { d =>  if (d >= threshold) g1 else g2 }
  }


  /*
   * Exercise 8.12
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}
