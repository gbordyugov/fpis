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
  case class JArray(get: List[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

/*
 * Exercise 9.9
 */
object JSONParser {
  def jsonParser[Parser[+_]](p: Parsers[Parser]): Parser[JSON] = {
    import JSON._
    import p.{string => _, _}

    /*
     * this promotes all strings to tokenized string parsers
     */
    implicit def tok(s: String) = p.string(s).token

    def nullParser: Parser[String] = "null"
    def jsonNull: Parser[JSON] = nullParser.map{_ => JNull}

    def jsonNumber: Parser[JNumber] =
      "\\d*.\\d*".r.map(_.toDouble).map(JNumber(_))

    def jsonString: Parser[JString] =
      (char('"') *> "[^\"]+".r <* char('"'))
        .map(_.mkString).map(JString(_)).token

    def jsonTrue:  Parser[JBool] = "true"  *> succeed(JBool(true))
    def jsonFalse: Parser[JBool] = "false" *> succeed(JBool(false))
    def jsonBool: Parser[JBool] = jsonTrue or jsonFalse

    def jsonAtom: Parser[JSON] = jsonNull or jsonBool or jsonNumber or jsonString

    def jsonArray: Parser[JArray] = sep(json, ",").map(JArray(_))

    def jsonQuotedString: Parser[String] =
      "\"" *> "[^\"]+".r <* "\""

    def jsonKeyValue: Parser[(String, JSON)] =
      map2(jsonQuotedString, (":" *> json))((_, _))

    def jsonObject: Parser[JObject] =
      sep(jsonKeyValue, ",").map(pairs => JObject(pairs.toMap))

    def json: Parser[JSON] = jsonAtom or jsonArray or jsonObject

    json
  }
}
