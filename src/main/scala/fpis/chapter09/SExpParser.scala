package fpis.chapter09.SimpleParser

import scala.util.matching.Regex

import fpis.chapter09.Parsers

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
