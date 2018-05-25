object StrictnessAndLazyness {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond)
      onTrue()
    else
      onFalse()

  def if2_[A](cond: Boolean, onTrue: => A, onFalse: => A):  A =
    if (cond)
      onTrue
    else
      onFalse

  def maybeTwice(b: Boolean, i: => Int) =
    if (b)
      i + i
    else
      0

  def maybeTwice2(b: Boolean, i: => Int) = {
    /*
     * `lazy` keword delays the evaluation of the rhs of that lazy
     * declaration and caches the result of the evaluation
     */
    lazy val j = i
    if (b)
      j + j
    else
      0
  }

  sealed trait Stream[+A]
  case object Empty extends Stream[Nothing]
  // the explicit thunks are a technical necessity here
  case class Cons[+A](h: () => A, t: () => Stream[A])
    extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty)
        empty
      else
        cons(as.head, apply(as.tail: _*))
    }
}
