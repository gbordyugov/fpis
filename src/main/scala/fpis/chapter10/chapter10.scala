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


  /*
   * Exercise 10.2
   */

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    // this would work too
    // def op(x: Option[A], y: Option[A]) = y orElse x
    def zero = None
  }


  /*
   * Exercise 10.3
   */

  def endoMonoid[A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = x => f(g(x))
    def zero = x => x
  }


  /*
   * Exercise 10.4 skipped (haven't read Part 2 yet)
   */


  /*
   * Exercise 10.5
   */

  def foldMap[X, Y] (xs: List[X], m: Monoid[Y])(f: X => Y): Y =
    xs.map(f).foldLeft(m.zero)(m.op)


  /*
   * Exercise 10.6 skipped
   */
}