package fpis.chapter09.JSONParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.{SimpleParser, SimpleParsers}
import fpis.chapter09.JSONParser.JSONParser._
import fpis.chapter09.JSONParser.JSON._



class JSONParserTest extends FlatSpec {
  import SimpleParsers.{run => runParser, succeed => parseSucceed, _}

   val parser = jsonParser(SimpleParsers)
   import parser._

  "JSONParser" should "be able parse true" in {
    val result = runParser(parser)("true")
    assert(result === Right(JBool(true)))
  }

  it should "be able parse false" in {
    val result = runParser(parser)("false")
    assert(result === Right(JBool(false)))
  }

  it should "be able parse a double" in {
    val result = runParser(parser)("5.9")
    assert(result === Right(JNumber(5.9)))
  }

  it should "be able to parse a string" in {
    val result = runParser(parser)("\"string\"")
    assert(result === Right(JString("string")))
  }
}
