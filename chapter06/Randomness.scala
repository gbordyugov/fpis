object Randomness {
  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }


  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  def randomPair(rng1: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng1.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }


  /*
   * Exercise 6.1
   */

  def nonNegativeInt(rng1: RNG): (Int, RNG) = {
    val (i, rng2) = rng1.nextInt
    i match {
      case j if j >= 0          => (j,  rng2)
      case j if j> Int.MinValue => (-j, rng2)
      case _                    => (0,  rng2)
    }
  }


  /*
   * Exercise 6.2
   */

  def double(rng1: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng1)
    (i.toDouble / Int.MaxValue.toDouble, rng2)
  }


  /*
   * Exercise 6.3
   */

  def intDouble(rng1: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng1.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng1: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng1)
    ((d, i), rng2)
  }

  def double3(rng1: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng1)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }


  /*
   * Exercise 6.4
   */

  def ints(count: Int)(rng1: RNG): (List[Int], RNG) = count match {
    case 0 => (List.empty: List[Int], rng1)
    case n => {
      val (ls, rng2) = ints(n - 1)(rng1)
      val (i,  rng3) = rng2.nextInt
      ((i::ls), rng3)
    }
  }


  /*
   * note the pattern RNG => (A, RNG)
   */

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt


  /*
   * Aim: to implement a combinator library, i.e. a couple of basic
   * abstractions plus means of combinations
   */

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def anotherUnit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng1 => {
      val (a, rng2) = s(rng1)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


  /*
   * Exercise 6.5
   */

  def newDouble: Rand[Double] =
    map(nonNegativeInt){ _ / Int.MaxValue.toDouble}


  /*
   * Exercise 6.6
   */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])
    (f: (A, B) => C): Rand[C] = rng1 => {
    val (a, rng2) = ra(rng1)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }
}
