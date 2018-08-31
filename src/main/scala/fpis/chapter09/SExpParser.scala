package fpis.chapter09.SimpleParser

import scala.util.matching.Regex

import fpis.chapter09.Parsers

sealed trait Atom
object Atom {
  type MyInt    = scala.Int
  type MyDouble = scala.Double
  type MyString = java.lang.String

  case class Int(value: MyInt) extends Atom
  case class Double(value: MyDouble) extends Atom
  case class String(value: MyString) extends Atom
  case class Symbol(name: MyString) extends Atom
}


object AtomTest {
  val i = Atom.Int(3)
  val s = Atom.String("string")
}


sealed trait SExp[A] {
  case class Atom[A](value: A) extends SExp[A]
  case class List[A](elements: List[SExp[A]]) extends SExp[A]
}

class SimpleParseError

case class SimpleParser[+A](run: String => Either[SimpleParseError, A]) {
}

object TestingSimpleParser {
  val parsers: Parsers[SimpleParseError, SimpleParser] = new Parsers[SimpleParseError, SimpleParser] {
    def run[A](p: SimpleParser[A])(input: String): Either[SimpleParseError, A] = p.run(input)
    def delay[A](p: => SimpleParser[A]): SimpleParser[A] = ???
    def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = ???
    def or[A](p: SimpleParser[A], q: => SimpleParser[A]): SimpleParser[A] = ???
    def regex(r: Regex): SimpleParser[String] = ???
    def slice[A](p: SimpleParser[A]): SimpleParser[String] = ???
    implicit def string(s: String): SimpleParser[String] = ???
  }
}
