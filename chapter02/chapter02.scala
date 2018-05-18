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
}
