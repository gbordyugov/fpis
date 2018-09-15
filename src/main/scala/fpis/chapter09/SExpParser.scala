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

import SimpleParserTest._

object MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
  def delay[A](p: => Parser[A]): Parser[A] = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def or[A](p: Parser[A], q: => Parser[A]): Parser[A] = ???
  def regex(r: Regex): Parser[String] = ???
  def slice[A](p: Parser[A]): Parser[String] = ???
  implicit def string(s: String): Parser[String] = ???
}
