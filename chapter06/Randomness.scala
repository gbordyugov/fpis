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
}
