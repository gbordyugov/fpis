package fpis.chapter15

/*
 * Transforms a stream containing I values into a stream containing O values
 */
sealed trait Process[I,O]

case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O])
    extends Process[I,O]

case class Await[I,O](recv: Option[I] => Process[I,O])
    extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]
