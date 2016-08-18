package parser

import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
  def main(args: Array[String]) {
//    println("input : "+ args(0))
//    println(parseAll(expr, args(0)))

    println(parseAll(expr, "2 * (3 + 7)"))
  }
}


class JSON extends JavaTokenParsers {
  def value : Parser[Any] = obj | arr |
    stringLiteral |
    floatingPointNumber |
    "null" | "true" | "false"
  def obj : Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
  def arr : Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
  def member: Parser[Any] = stringLiteral ~ ":" ~ value
}

object JSONMain extends JSON {
  def main(args: Array[String]) {
    println(parseAll(value, "{\"a\":\"b\"}"))
  }
}


class JSON1 extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)
  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"
  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^
      { case name ~ ":" ~ value => (name, value) }
  def value: Parser[Any] = (
    obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )
}

object JSON1Main extends JSON1 {

  val json =
    """
      |{
      |"address book": {
      |"name": "John Smith",
      |"address": {
      |"street": "10 Market Street",
      |"city" : "San Francisco, CA",
      |"zip" : 94111
      |},
      |"phone numbers": [
      |"408 338-4238",
      |"408 111-6892"
      |]
      |}
      |}
    """.stripMargin

  def main(args: Array[String]) {
    println(parseAll(value, json))
  }
}


// template ::= "{{" stringLiteral " " stringLiteral { "|" member } "}}"
// member ::= stringLiteral "=" stringLiteral



class WikiText extends RegexParsers {
  def obj: Parser[(String, Any)] = "{{Infobox"~infobox~"|"~members~"}}" ^^ { case "{{Infobox"~infobox~"|"~members~"}}" => (infobox, members) }
//  def obj: Parser[(String, Any)] = "{{"~infobox~"|"~members~"}}" ^^ { case "{{"~infobox~"|"~members~"}}" => (infobox, members) }
  def members: Parser[Map[String, Any]] = repsep(member, "|") ^^ (Map() ++ _)
  def member: Parser[(String, Any)] = str~"="~str ^^ { case name~"="~value => (name, value) }
  def infobox: Parser[String] = """\s*([\w\s]+)""".r
  def str: Parser[String] = """[\w\s\.]+""".r ~ template ^^ { case s~t => s ++ t }
  def template: Parser[String] = """{{\w+}}""".r
}

object WikiMain extends WikiText {

  val PAGE1 =
    """
      |{{Infobox person
      | | name             = Alfred Korzybski
      | | image            = Alfred Korzybski.jpg
      | | image_size       = 191x238px
      | | birth_date       = {{birth date|1879|7|3|mf=y}}
      | | birth_place      = [[Warsaw]], [[Vistula Country]], [[Russian Empire]]
      | | death_date       = {{death date and age|1950|3|1|1879|7|3|mf=y}}
      | | death_place      = [[Lakeville, Connecticut]], U.S.
      | | occupation       = [[Engineer]], [[philosopher]], [[mathematician]]
      | | spouse           =
      |}}
      |
    """.stripMargin

  def main(args: Array[String]) {
    println(parseAll(obj, "{{Infobox dummy | asd = sdf}}"))

    val result = parseAll(obj, "{{Infobox dummy | asd = sdf}}")
    println(result.get._1)
    println(result.get._2)

    println(parseAll(obj, PAGE1))
  }
}