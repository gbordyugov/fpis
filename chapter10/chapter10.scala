object Chapter10 {
  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero = ""
  }

  /*
   * Exercise 10.1
   */

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    def zero = 0
  }
}
