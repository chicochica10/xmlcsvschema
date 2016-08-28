/**
  * Created by cloudera on 8/26/16.
  */


import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers{

  ///// logical grammar
  /*
  <expression>::=<term>{<or><term>}
  <term>::=<factor>{<and><factor>}
  <factor>::=<constant>|<not><factor>|(<expression>)
  <constant>::= false|true
  <or>::='|'
  <and>::='&'
  <not>::='!'
  */

  val or = "OR"
  val and = "AND"
  val not = "NOT"
  val openPar = "("
  val closePar = ")"

//  def words: Parser[String] = """[\[\]\(\)',A-Za-z0-9]+""".r ^^ {_.toString}
  def words: Parser[String] = """[\[\]\(\)',\d\w]+""".r ^^ {_.toString}
 // def words: Parser[String] = """[^><=!]+""".r ^^ {_.toString}

  def booleanOperator: Parser[String] = ("==" | "!=" | ">=" | "<=" | ">" | "<")
  def booleanValue: Parser[String] = ("TRUE" | "FALSE")

  def booleanTerm: Parser[String] = (booleanValue | (words ~ booleanOperator ~ words)) ^^ {
    case ws1 ~ "==" ~ ws2 =>  ws1 + " equals " + ws2
    case ws1 ~ "!=" ~ ws2 =>  ws1 + " distinct " + ws2
    case ws1 ~ ">=" ~ ws2 => ws1 + " greater or equal than " + ws2
    case ws1 ~ "<=" ~ ws2 => ws1 + " less or equal than " + ws2
    case ws1 ~ ">" ~ ws2 => ws1 + " greater than " + ws2
    case ws1 ~ "<" ~ ws2 => ws1 + " less than " + ws2
    case bv => bv.toString.toLowerCase
  }

  //def constant: Parser[String] = ("false" | "true") ^^ {_.toString}
  def factor: Parser[String] = (booleanTerm | not ~ factor | openPar ~ expr ~ closePar) ^^ {
    case oPar ~ exp1 ~ cPar => "(" + exp1 + ")"
    case not ~ factor1 => "not " + factor1
    case bt => {
      (""+bt).toLowerCase
    }
  }

  def term: Parser[String] = (factor ~ opt( and ~ factor)) ^^ {
    case factor1 ~ None => factor1
    case factor1 ~ Some (and ~ factor2) => factor1 + " and " + factor2
  }

  def expr: Parser[String] = (term ~ opt(or ~ term)) ^^ {
    case term1 ~ None =>  term1
    case term1 ~ Some (or ~ term2) => term1 + " or " + term2
  }


}

object LolSimpleParser extends SimpleParser {

  def main(args: Array[String]) = {

    val ini= "[FECHA]>='January1,1970' AND FECHA<=getdate()"
    val result = parseAll (expr, ini)
    result match {
      case Success (matched, _) => println (matched)
      case Failure (msg, _) => println ("FAILURE: " + msg)
      case Error (msg, _) => println ("ERROR: " + msg)
    }
  }

  def getCheckConstraints (check: String) = {
    val result = parseAll (expr, check)
    result match {
      case Success (matched, _) => matched
      case Failure (msg, _) => {println ("FAILURE: " + msg); ""}
      case Error (msg, _) => {println ("ERROR: " + msg); ""}
    }
  }

}