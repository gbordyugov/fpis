object Chapter10 {
  trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(x: String, y: String) = x + y
    def zero = ""
  }
}
