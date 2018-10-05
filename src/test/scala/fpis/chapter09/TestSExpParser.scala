package fpis.chapter09.SExpParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.{SimpleParser, SimpleParsers}
import fpis.chapter09.SExpParser.SExpParser._


object AtomTest {
  val i = AtomInt(3)
  val s = AtomString("string")
}

class SExpParserTest extends FlatSpec {
  import SimpleParsers.{run => runParser, succeed => parseSucceed, _}
  val parser = sExpParser(SimpleParsers)
  import parser._

  "sExpParser" should "be able to parse simple symbols" in {
    val result = runParser(parser)("abcd")
    println(result)
  }

  it should "be able to parse simple integers" in {
    val result = runParser(parser)("2345")
    println(result)
  }

  it should "be able to parse simple strings" in {
    val result = runParser(parser)("\"abcd\"")
    println(result)
  }
}
