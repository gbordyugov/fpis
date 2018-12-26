package fpis.chapter09.JSONParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.{SimpleParser, SimpleParsers}
import fpis.chapter09.JSONParser.JSONParser._
import fpis.chapter09.JSONParser.JSON._


/*
 * Exercise 9.15
 */

class JSONParserTest extends FlatSpec {
  import SimpleParsers.{run => runParser, succeed => parseSucceed, _}

   val parser = jsonParser(SimpleParsers)
   import parser._

  "JSONParser" should "be able to parse true" in {
    val result = runParser(parser)("true")
    assert(result === Right(JBool(true)))
  }

  it should "be able to parse false" in {
    val result = runParser(parser)("false")
    assert(result === Right(JBool(false)))
  }

  it should "be able to parse an int" in {
    val result = runParser(parser)("5")
    assert(result === Right(JNumber(5)))
  }

  it should "be able to parse a double" in {
    val result = runParser(parser)("5.9")
    assert(result === Right(JNumber(5.9)))
  }

  it should "be able to parse a double with a leading dot" in {
    val result = runParser(parser)(".9")
    assert(result === Right(JNumber(0.9)))
  }

  it should "be able to parse a string" in {
    val result = runParser(parser)("\"string\"")
    assert(result === Right(JString("string")))
  }

  it should "be able to parse an empty array" in {
    val result = runParser(parser)("[]")
    assert(result === Right(JArray(List(): List[JSON])))
  }

  it should "be able to parse an array" in {
    val result = runParser(parser)("[1.0, 2, 3]")
    assert(result ===
      Right(JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0)))))
  }

  it should "be able to parse an empty object" in {
    val result = runParser(parser)("{}")
    assert(result === Right(JObject(Map(): Map[String,JSON])))
  }

  it should "be able to parse an object with one field" in {
    val result = runParser(parser)("{ \"bla\": 5}")
    assert(result === Right(JObject(Map(("bla", JNumber(5))))))
  }

  it should "be able to parse an object with two fields" in {
    val result = runParser(parser)("{ \"bla\": 5, \"bli\": \"blo\"}")
    assert(result ===
      Right(JObject(Map(("bla", JNumber(5)), ("bli", JString("blo"))))))
  }

  it should "be able to parse a slightly more complex object" in {
    val string = "{ \"bla\": 5, \"bli\": \"blo\", \"blu\": [1, 2, {}], \"blk\": {\"a\": 1}}"
    val result = runParser(parser)(string)
    result match {
      case Right(_) => assert(true)
      case Left(_) => assert(false)
    }
  }

  it should "be able to parse JSON schema" in {
    val schema = """{
  "$schema": "http://json-schema.org/schema#",
  "title": "Product",
  "type": "object",
  "required": ["id", "name", "price"],
  "properties": {
    "id": {
      "type": "number",
      "description": "Product identifier"
    },
    "name": {
      "type": "string",
      "description": "Name of the product"
    },
    "price": {
      "type": "number",
      "minimum": 0
    },
    "tags": {
      "type": "array",
      "items": {
        "type": "string"
      }
    },
    "stock": {
      "type": "object",
      "properties": {
        "warehouse": {
          "type": "number"
        },
        "retail": {
          "type": "number"
        }
      }
    }
  }
}
"""
    val result = runParser(parser)(schema)
    result match {
      case Right(_) => assert(true)
      case Left(_) => assert(false)
    }
  }
}
