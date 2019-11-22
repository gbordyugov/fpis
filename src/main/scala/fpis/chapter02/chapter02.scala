package fpis.chapter02

object Chapter2 {

  object MyModule {
    def abs(n: Int): Int =
      if (n < 0) -n
      else n

    private def formatAbs(x: Int) = {
      val msg = "The absolute value of %d is %d"
      msg.format(x, abs(x))
    }

    def main(args: Array[String]): Unit =
      println(formatAbs(-42))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }


  /*
   * Exercise 2.1
   */

  def fib(n: Int): Int = {
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if (n <= 0)
        acc1
      else
        go(n - 1, acc2, acc1 + acc2)

    go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length)
        -1
      else
        if (p(as(n)))
          n
        else
          loop(n + 1)

    loop(0)
  }


  /*
   * Exercise 2.2
   */

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) // zero- and one-length arrays are sorted
        true
      else // we have at least two elements
        if (ordered(as(n), as(n+1)))
          loop(n + 1)
        else
          false

    loop(0)
  }

  val lessThan = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int): Boolean = a < b
  }


  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)


  /*
   * Exercise 2.3
   */

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  /*
   * Exercise 2.3 (Aaron's try)
   */

  def curryAaron[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)


  /*
   * Exercise 2.4
   */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  /*
   * Exercise 2.5
   */

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
