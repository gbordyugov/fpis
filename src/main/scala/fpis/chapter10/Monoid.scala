package fpis.chapter10

trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

object Chapter10 {
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
   * for Exercise 10.4 see TestMonoid.scala
   */

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /*
   * Exercise 10.5
   */

  def foldMap[X, Y] (xs: List[X], m: Monoid[Y])(f: X => Y): Y =
    concatenate(xs.map(f), m)


  /*
   * Exercise 10.6
   */

  def foldLeft[A, B](a: A)(bs: List[B])(f: (A, B) => A): A = {
    val funcs: List[A => A] = bs.map(b => (a: A) => f(a, b))
    concatenate(funcs, endoMonoid[A])(a)
  }

  def foldRight[A, B](b: B)(as: List[A])(f: (A, B) => B): B = {
    val funcs: List[B => B] = as.map(a => (b: B) => f(a, b))
    /*
     * this should be concatenateRight, but I think it's quite straight-forward
     */
    concatenate(funcs, endoMonoid[B])(b)
  }

  /*
   * Exercise 10.7
   */

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???
}
