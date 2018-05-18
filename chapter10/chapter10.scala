object Chapter10 {
  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(x: List[A], y: List[A]) = x ++ y
    def zero = Nil
  }

  /*
   * Exercise 10.1
   */

  val intAddition = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x | y
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x & y
    def zero = true
  }
}
