package fpis.chapter14

sealed trait ST[S,A] { self =>
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
