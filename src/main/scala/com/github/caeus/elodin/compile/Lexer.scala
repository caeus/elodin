package com.github.caeus.elodin.compile

import com.github.caeus.elodin.compile.ElodinToken.{Bool, Operator}
import ElodinToken._
import com.github.caeus.plutus.PackerSyntax.StringPackerSyntax
import com.github.caeus.plutus.{Packer, PrettyPacker}
import com.jsoniter.Jsoniter
import jdk.nashorn.internal.ir.annotations.Ignore
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
  case object Dot                        extends ElodinToken
  case class Reference(to: String)       extends ElodinToken
  case class Operator(to: String)        extends ElodinToken
  case object Do                         extends ElodinToken
  case object Let                        extends ElodinToken
  case object Fun                        extends ElodinToken
  case object Curly                      extends Delimiter
  case object Equals                     extends ElodinToken
  case object Import                     extends ElodinToken
  case object Comma                      extends ElodinToken
  case object Semicolon                  extends ElodinToken
  case class IntNum(value: BigInt)       extends ElodinToken
  case class FloatNum(value: BigDecimal) extends ElodinToken
  case class Bool(value: Boolean)        extends ElodinToken
  case class Text(value: String)         extends ElodinToken
  case object Unit_                      extends ElodinToken

}
trait Lexer {
  def lex(code: String): Task[Vector[ElodinToken]]
}
object Lexer {
  def make: Lexer = new DefaultLexer
}
final class DefaultLexer extends Lexer {
  import StringPackerSyntax._

  private val opChars = """|^&=!<>:+-*/""".toSet

  lazy val digits: Packer[String, Char, Unit] = P(_.isDigit).rep(min = 1).as(())

  lazy val exponent: Packer[String, Char, Unit] = (P("e|E".r) ~ P("""[+\-]""".r).? ~ digits).as(())

  lazy val fractional: Packer[String, Char, Unit] = P(".") ~ digits

  lazy val integral: Packer[String, Char, Unit] = ((P("0") | P("""[1-9]""".r)) ~ digits.?).as(())

  lazy val _comment: Packer[String, Char, Unit] = P("*/") | (P(_ => true) ~ _comment).as(())
  lazy val comment: Packer[String, Char, Unit]  = (P("/*") ~ _comment)

  lazy val booleanToken: Packer[String, Char, Bool] =
    P("""true|false""".r).map(_.toBoolean).map(Bool.apply)

  lazy val integralToken: Packer[String, Char, IntNum] =
    (P("""[+\-]""".r).? ~ integral).!.map(_.value).map { num =>
      IntNum(BigInt(num))
    }

  lazy val floatingToken: Packer[String, Char, FloatNum] =
    (P("""[+\-]""".r).? ~ integral ~ fractional.?.as(()) ~ exponent.?.as(())).!.map(_.value)
      .map(num => FloatNum(BigDecimal(num)))

  lazy val opToken = P(c => opChars.contains(c)).rep.!.map(_.value).map(Operator.apply)

  lazy val refToken: Packer[String, Char, Reference] =
    (P(c => c.isLetter || c == '_').!.map(_.value) ~ fromPartial {
      case char if char.isLetterOrDigit || char == '_' => char
    }.rep.!.map(_.value))
      .map {
        case (a: String, b: String) =>
          Reference(a ++ b)
      }

  lazy val hexDigit: Packer[String, Char, String] = P("[0-9a-fA-F]".r)

  lazy val unicodeEscape: Packer[String, Char, Unit] =
    (P("u") ~ hexDigit ~
      hexDigit ~ hexDigit ~ hexDigit).as(())

  lazy val escape: Packer[String, Char, Unit] = P("\\") ~ (P("[\"/\\\\bfnrt]".r) | unicodeEscape)
    .as(())

  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  lazy val strChars: Packer[String, Char, String] = P(stringChars _).rep.!.map(_.value)

  lazy val textToken: Packer[String, Char, Text] =
    (P("\"") ~ (strChars | escape).rep ~ P("\"")).!.map { window =>
      Text(Jsoniter.parse(window.value).readString())
    }

  private val lexerPacker: Packer[String, Char, Vector[ElodinToken]] =
    (P("(").as(Some(Parenthesis.Open)) |
      P(")").as(Some(Parenthesis.Close)) |
      P("=>").as(Some(Fun)) |
      P("""\s+""".r).as(None) |
      P("[").as(Some(Bracket.Open)) |
      P("]").as(Some(Bracket.Close)) |
      P("{").as(Some(Curly.Open)) |
      P("}").as(Some(Curly.Close)) |
      P("<-").as(Some(Yield)) |
      P("do").as(Some(Do)) |
      P("let").as(Some(Let)) |
      P("import").as(Some(Import)) |
      P("=").as(Some(Equals)) |
      P(",").as(Some(Comma)) |
      P(";").as(Some(Semicolon)) |
      P("unit").as(Some(Unit_)) |
      P(".").as(Some(Dot)) |
      comment.as(None) | comment.as(None) |
      opToken.map(Some.apply) |
      booleanToken.map(Some.apply) |
      refToken.map(Some.apply) |
      integralToken.map(Some.apply) |
      floatingToken.map(Some.apply) |
      textToken.map(Some.apply)).rep
      .map(_.collect[ElodinToken] {
        case Some(x) => x
      }) ~ End
  lazy val prettyPacker = PrettyPacker.version1(lexerPacker)

  def lex(source: String): Task[Vector[ElodinToken]] =
    Task.effectSuspend(Task.fromEither(prettyPacker.process(source)))

}
