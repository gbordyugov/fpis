package fpis.chapter09.JSONParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.{SimpleParser, SimpleParsers}
import fpis.chapter09.JSONParser.JSONParser._


class JSONParserTest extends FlatSpec {
  import SimpleParsers.{run => runParser, succeed => parseSucceed, _}

  /*
   val parser = jsonParser(SimpleParsers)
   import parser._
   */

  "JSONParser" should "do stuff" in {
    println("everything ok!")
  }
}
