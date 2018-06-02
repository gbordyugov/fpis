object Parallel {
  trait Par[A]


  /*
   * Exercise 7.1
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}
