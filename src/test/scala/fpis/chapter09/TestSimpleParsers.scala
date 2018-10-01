package fpis.chapter09.SimpleParsers

import org.scalatest._

object TestMySimpleParsers {
  import SimpleParsers._
  import SimpleParser.Parser

  def abra: Parser[String] = "abra"

  val succ = run(abra)("abra")
  val fail = run(abra)("abr")

  def re: Parser[String] = regex("a+".r)

  val succ_re = run(re)("abra")
  val fail_re = run(re)("nopes")

  def number: Parser[String] = regex("[0-9]+".r)
  def a: Parser[String] = "a"
  def nAs: Parser[Int] = number.map(_.toInt)

  val flatMapSuccTest = run(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaaa")
  val flatMapFailTest = run(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaa")

  val parseManyA  = run("a".many )("aaaaa")
  val parseMany1A = run("a".many1)("aaaaa")
}


class SimpleParsersTest extends FlatSpec {
  "bla" should "blo" in {
  }
}
