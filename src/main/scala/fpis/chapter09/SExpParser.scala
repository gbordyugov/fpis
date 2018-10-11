package fpis.chapter09.SExpParser

import scala.language.{higherKinds, implicitConversions}

import fpis.chapter09.{Parsers, ParseError, Location}
import fpis.chapter09.SimpleParsers.SimpleParsers

sealed trait Atom
case class AtomInt   (value: Int   ) extends Atom
case class AtomDouble(value: Double) extends Atom
case class AtomString(value: String) extends Atom
case class AtomSymbol(name:  String) extends Atom

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

    def atomInt: Parser[AtomInt] =
      int.map(AtomInt(_)).token

    def atomDouble: Parser[AtomDouble] =
      raw"(\d+\.\d*|\.\d+|\d+)".r.map(_.toDouble).map(AtomDouble(_)).token

    def atomString: Parser[AtomString] =
      (char('"') *> "[^\"]+".r <* char('"'))
        .map(_.mkString).map(AtomString(_)).token

    def atomSymbol: Parser[AtomSymbol] =
      letter.many1.map(_.mkString).map(AtomSymbol(_)).token

    def atom: Parser[Atom] =
      atomInt or atomDouble or atomString or atomSymbol

    def sExpAtom: Parser[SExpAtom[Atom]] =
      atom.map(SExpAtom(_))

    def sExpList: Parser[SExpList[Atom]] =
      ("(" *> sep(sExp, whitespace) <* ")").map(SExpList(_))

    def sExp = sExpAtom or sExpList

    sExp
  }
}
