object PropertyBasedTesting {
  /*
   * Exercise 8.1
   *
   * sum of the reverse should be equal to the sum of the list
   * sum of N repetitions of a number P should be equal NxP
   * sum of an non-empty list should be equal to the sum of its head
   *   plus the sum of the tail
   * sum of a list should be equal to the sum of the sums of its partitions
   * sum of any permutation of the list should be the same
   */

  /*
   * Exercise 8.2
   *
   * maximum should not be smaller than any element of the list
   * maximum of the reversed list should be equal to the maximum of
   *   the list
   * max of any permutation should be the same
   * max of a non-empty list should be max(head, max(tail))
   */

  trait Prop {
    def check: Unit
    def &&(p: Prop): Prop
  }

  /*
   * Exercise 8.3
   */
  object Exercise83 {
    trait Prop { outer =>
      def check: Boolean
      def &&(that: Prop): Prop = new Prop {
        def check = outer.check && that.check
      }
    }
  }
}
