package fpis.chapter09.SExpParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.{SimpleParser, SimpleParsers}
import fpis.chapter09.SExpParser.SExpParser._


class SExpParserTest extends FlatSpec {
  import SimpleParsers.{run => runParser, succeed => parseSucceed, _}
  val parser = sExpParser(SimpleParsers)
  import parser._

  type SExpression = SExp[Atom]

  val abcdSymbol = AtomSymbol("abcd")
  val abcdString = AtomString("abcd")
  val intAtom    = AtomInt(2345)
  val emptyList  = SExpList(List(): List[SExpression])

  val one = AtomInt(1)
  val two = AtomInt(2)
  val tre = AtomInt(3)

  val quoteSymbol = AtomSymbol("quote")

  "sExpParser" should "be able to parse simple symbols" in {
    val result = runParser(parser)("abcd")
    assert(result === Right(SExpAtom(abcdSymbol)))
  }

  it should "be able to parse simple integers" in {
    val result = runParser(parser)("2345")
    assert(result === Right(SExpAtom(intAtom)))
  }

  it should "be able to parse simple strings" in {
    val result = runParser(parser)("\"abcd\"")
    assert(result === Right(SExpAtom(abcdString)))
  }

  it should "be able to parse empty lists" in {
    val result = runParser(parser)("()")
    assert(result === Right(emptyList))
  }

  it should "be able to parse simple lists" in {
    val result = runParser(parser)("(1 2 3)")
    assert(result === Right(SExpList(List(SExpAtom(one), SExpAtom(two),
      SExpAtom(tre)))))
  }

  it should "be able to parse nested lists" in {
    val result = runParser(parser)("(1 (quote 2 (3)))")
    val uno: SExpression = SExpList(List(SExpAtom(AtomInt(3))))
    val due: SExpression = SExpList(List(SExpAtom(quoteSymbol),
      SExpAtom(AtomInt(2)), uno))
    val tre: SExpression = SExpList(List(SExpAtom(AtomInt(1)), due))

    assert(result === Right(tre))
  }
}
