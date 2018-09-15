package fpis.chapter09.SimpleParser

import scala.util.matching.Regex

import fpis.chapter09.{Parsers, ParseError, Location}

sealed trait Atom
case class AtomInt   (value: Int   ) extends Atom
case class AtomDouble(value: Double) extends Atom
case class AtomString(value: String) extends Atom
case class AtomSymbol(name:  String) extends Atom


object AtomTest {
  val i = AtomInt(3)
  val s = AtomString("string")
}

/*
 parameterized by the atom type
 */
sealed trait SExp[A]
case class SExpAtom[A](value: A) extends SExp[A]
case class SExpList[A](elements: List[SExp[A]]) extends SExp[A]

object SimpleParserTest {
  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]
}

object TestingSimpleParser {
  import SimpleParserTest._
  import SimpleParserTest.{Parser => SimpleParser}

  val parsers: Parsers[Parser] = new Parsers[SimpleParser] {
    def run[A](p: SimpleParser[A])(input: String): Either[ParseError, A] = ???
    def delay[A](p: => SimpleParser[A]): SimpleParser[A] = ???
    def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = ???
    def or[A](p: SimpleParser[A], q: => SimpleParser[A]): SimpleParser[A] = ???
    def regex(r: Regex): SimpleParser[String] = ???
    def slice[A](p: SimpleParser[A]): SimpleParser[String] = ???
    implicit def string(s: String): SimpleParser[String] = ???
  }
}
