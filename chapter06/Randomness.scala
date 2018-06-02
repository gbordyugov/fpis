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


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)


  /*
   * Exercise 6.7
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // val zero: Rand[List[A]] = rng => (List.empty, rng)
    val zero = unit[List[A]](List.empty)
    fs.foldRight(zero)((a, b) => map2(a, b)(_ :: _))
  }


  /*
   * A pattern is beginning to emerge: we're progressing towards
   * implementations that don't explicitly mention or pass along
   * the RNG value
   */


  /*
   * Exercise 6.8
   */

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng1 => {
    val (a, rng2) = f(rng1)
    val (b, rng3) = g(a)(rng2)
    (b, rng3)
  }


  /*
   * Exercise 6.9
   */

  def mapByFlatMap[A, B](a: Rand[A])(f: A => B): Rand[B] =
    flatMap(a)(a => unit[B](f(a)))

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])
    (f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit[C](f(a, b))))



  /*
   * Exercise 6.10
   */

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State { s1 => {
        val (a, s2) = run(s1)
        (f(a), s2)
      }
    }

    def flatMap[B] (f: A => State[S, B]): State[S, B] =
      State { s1 => {
        val (a, s2) = run(s1)
        f(a).run(s2)
      }
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])
      (f: (A, B) => C): State[S, C] = State { s1 => {
        val (a, s2) = sa.run(s1)
        val (b, s3) = sb.run(s2)
        (f(a, b), s3)
      }
    }

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List.empty)) { (a, b) =>
        map2(a, b)(_ :: _)
      }
  }


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  /*
   * Exercise 6.13
   */

  trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def update(i: Input, m: Machine): Machine = (i, m) match {
    case (_, Machine(_, 0, _)) => m
    case (Turn, Machine(true,  _, _)) => m
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(false, ca, co)) => Machine(true, ca-1, co)
    case (Coin, Machine(true, ca, co)) if ca > 0 =>
      Machine(false, ca, co+1)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = {
    val actions = inputs.map { i =>
      State[Machine, (Int, Int)] { m => {
          val nm = update(i, m)
          ((nm.candies, nm.coins), nm)
        }
      }
    }
    ???
  }
}
