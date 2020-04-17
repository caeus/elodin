package com.github.caeus.elodin.frontend

import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.plutus.Packer
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.SyntaxSugar.StringSyntaxSugar
import com.github.caeus.plutus.syntax._
import com.jsoniter.Jsoniter
import zio.Task

//import com.github.caeus.plutus.syntax._
sealed trait ElodinToken
object ElodinToken {
  sealed trait Delimiter { del =>
    case object Open extends ElodinToken {
      override def toString: String = s"$del.Open"
    }
    case object Close extends ElodinToken {
      override def toString: String = s"$del.Closed"
    }
  }
  case object Parenthesis                extends Delimiter
  case object Bracket                    extends Delimiter
  case object Yield                      extends ElodinToken
  case class Reference(to: String)       extends ElodinToken
  case object Do                         extends ElodinToken
  case object Let                        extends ElodinToken
  case object Fn                         extends ElodinToken
  case object Curly                      extends Delimiter
  case object Colon                      extends ElodinToken
  case class IntNum(value: BigInt)       extends ElodinToken
  case class FloatNum(value: BigDecimal) extends ElodinToken
  case class Bool(value: Boolean)        extends ElodinToken
  case class Text(value: String)         extends ElodinToken
  case object Ignore                     extends ElodinToken

}

class Lexer {
  import StringSyntaxSugar._

  lazy val digits: Packer[String, Char, Unit] = P(_.isDigit).rep(min = 1).as(())

  lazy val exponent: Packer[String, Char, Unit] = (P("e|E".r) ~ P("""[+\-]""".r).? ~ digits).as(())

  lazy val fractional: Packer[String, Char, Unit] = P(".") ~ digits

  lazy val integral: Packer[String, Char, Unit] = ((P("0") | P("""[1-9]""".r)) ~ digits.?).as(())

  lazy val booleanToken: Packer[String, Char, Bool] =
    P("""true|false""".r).map(_.toBoolean).map(Bool.apply)

  lazy val integralToken: Packer[String, Char, IntNum] =
    integral.!.map(_.value).map { num =>
      IntNum(BigInt(num))
    }

  lazy val floatingToken: Packer[String, Char, FloatNum] =
    (P("""[+\-]""".r).? ~ integral ~ fractional.?.as(()) ~ exponent.?.as(())).!.map(_.value)
      .map(num => FloatNum(BigDecimal(num)))

  lazy val refToken: Packer[String, Char, Reference] =
    (P(_.isLetter).!.map(_.value) ~ fromPartial {
      case char if char.isLetterOrDigit => char
    }.rep.!.map(_.value))
      .map {
        case (a: String, b: String) =>
          Reference(a + b)
      }

  lazy val hexDigit: Packer[String, Char, String] = P("[0-9a-fA-F]".r)

  lazy val space: Packer[String, Char, String] = P("[ \r\n]*".r)

  lazy val unicodeEscape: Packer[String, Char, Unit] =
    (P("u") ~ hexDigit ~
      hexDigit ~ hexDigit ~ hexDigit).as(())

  lazy val escape: Packer[String, Char, Unit] = P("\\") ~ (P("[\"/\\\\bfnrt]".r) | unicodeEscape)
    .as(())

  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  lazy val strChars: Packer[String, Char, String] = P(stringChars _).rep.!.map(_.value)

  lazy val stringToken =
    (P("\"").logging("QUOTES") ~ (strChars | escape).rep.logging("CNTNT") ~ P("\"")).!.map {
      window =>
        val value = window.value
        println(value)
        val text = Text(Jsoniter.parse(value).readString())
        println(text.value)
        text
    }.logging("STR")

  private val lexerPacker: Packer[String, Char, Vector[ElodinToken]] =
    (P("(").as(Some(Parenthesis.Open)) |
      P(")").as(Some(Parenthesis.Close)) |
      P("fn").as(Some(Fn)) |
      P("""\s+""".r).as(None) |
      P("[").as(Some(Bracket.Open)) |
      P("]").as(Some(Bracket.Close)) |
      P("{").as(Some(Curly.Open)) |
      P("}").as(Some(Curly.Close)) |
      P("<:").as(Some(Yield)) |
      P("do").as(Some(Do)) |
      P("let").as(Some(Let)) |
      P(":").as(Some(Colon)) |
      P("_").as(Some(Ignore)) |
      booleanToken.map(Some.apply) |
      refToken.map(Some.apply) |
      integralToken.map(Some.apply) |
      floatingToken.map(Some.apply) |
      stringToken.map(Some.apply))
    //.logging("AToken")
    .rep
      .map(_.collect[ElodinToken] {
        case Some(x) => x
      }) ~ End
  def lex(source: String): Task[Vector[ElodinToken]] =
    Task.effectSuspend(lexerPacker.take(source) match {
      case Done(result, _) =>
        Task.succeed(result)
      case f @ Failed(errors) => Task.fail(new Exception(s"Lexing error at : ${f.report(source)}"))
    })

}
