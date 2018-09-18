package fpis.chapter09

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
    import p._

    ???
  }
}
