package fpis.chapter09.SExpParser

import scala.language.{higherKinds, implicitConversions}

import fpis.chapter09.{Parsers, ParseError, Location}
import fpis.chapter09.SimpleParser.SimpleParsers

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

object SExpParser {
  type SExpression = SExp[Atom]

  def sExpParser[Parser[+_]](p: Parsers[Parser]): Parser[SExpression] = {
    import p.{string => _, _}

    /*
     * this promotes all strings to tokenized string parsers
     */
    implicit def tok(s: String) = p.string(s).token

    def openingParen: Parser[String] = "("
    def closingParen: Parser[String] = ")"
    def comma:        Parser[String] = ","

    def atomInt: Parser[AtomInt] =
      int.token.map(AtomInt(_))

    def atomDouble: Parser[AtomDouble] = ???

    def atomString: Parser[AtomString] = ???

    def atomSymbol: Parser[AtomSymbol] =
      digit.many1.map(_.mkString).map(AtomSymbol(_))

    def atom: Parser[Atom] =
      atomInt or atomDouble or atomString or atomSymbol

    def sExpAtom: Parser[SExpAtom[Atom]] = atom.map(SExpAtom(_))
    def sExpList: Parser[SExpList[Atom]] =
      (openingParen *> sep(sExp, comma) <* closingParen).map(SExpList(_))
    def sExp: Parser[SExpression] = sExpAtom or sExpList

    sExp
  }
}

object SExpParserTest {
  import SExpParser._
  val parser = sExpParser(SimpleParsers)
}
