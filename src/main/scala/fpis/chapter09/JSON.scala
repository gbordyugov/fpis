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

    def whitespace: Parser[String] = "\\s*".r

    def token[A](p: Parser[A]) = p <* whitespace

    def between[L, R, A](l: Parser[L], r: Parser[R],
      a: Parser[A]): Parser[A] = l *> a <* r

    def digit: Parser[Int] = "[0-9]".r.map(_.toInt)

    def letter: Parser[String] = "[a-Z_]".r

    def colon: Parser[String] = ":"

    def doubleQuote: Parser[String] = "\""

    def singleQuote: Parser[String] = "'"

    def singleQuoted[A](p: Parser[A]): Parser[A] =
      between(singleQuote, singleQuote, p)

    def doubleQuoted[A](p: Parser[A]): Parser[A] =
      between(doubleQuote, doubleQuote, p)

    ???
  }
}
