package fpis.chapter09.JSONParser

import scala.language.{higherKinds, implicitConversions}

import fpis.chapter09.{Parsers, ParseError, Location}
import fpis.chapter09.SimpleParsers.SimpleParsers

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

/*
 * Exercise 9.9
 */
object JSONParser {
  def jsonParser[Parser[+_]](p: Parsers[Parser]): Parser[JSON] = {
    import p.{string => _, _}

    /*
     * this promotes all strings to tokenized string parsers
     */
    implicit def tok(s: String) = p.string(s).token
    ???
  }
}
