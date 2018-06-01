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
}
