package fpis.chapter14

sealed trait ST[S,A] { self =>
  /*
   * note that the run() method is protected, contrarily to the normal
   * state monad
   */
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo: A = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
}

/*
 * observe that this trait is sealed and has no subclasses
 * it means that constructing instances of it is only possible by
 * calling the apply() method of the companion object
 */
sealed trait STRef[S,A] {
  protected var cell: A

  /*
   * methods read() and write() are pure: they don't do anything
   * themselves but rather return ST actions
   */
  def read: ST[S,A] = ST(cell)

  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  /*
   * we don't actually create any instances of STRef in apply() here
   * looks a bit counterintuitive, nicht wahr?
   *
   * what we create here is a ST[S,_] - it has the same type S as
   * the corresponding STRef[S,_] type. So this S type is a kind of
   * token or a pass to control who can access the state.
   */
  def apply[S,A](a: A): ST[S, STRef[S,A]] = {
    val ref: STRef[S,A] = new STRef[S,A] {
      var cell = a
    }
    ST(ref)
  }
}
