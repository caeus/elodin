package com.github.caeus.elodin.frontend

import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.plutus.Packer
import com.github.caeus.plutus.PackerResult.{Done, Failed}
import com.github.caeus.plutus.SyntaxSugar.StringSyntaxSugar
import com.github.caeus.plutus.syntax._
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
  case class Text(value: String)         extends ElodinToken

}

class Lexer {
  import StringSyntaxSugar._
  private val value = fromPartial {
    case char if char.isLetter => char
  }.!.map(_.value)

  private val refToken: Packer[String, Char, Option[Reference]] =
    (value ~ fromPartial {
      case char if char.isLetterOrDigit => char
    }.rep.!.map(_.value))
      .map {
        case (a: String, b: String) =>
          a + b
      }
      .map(to => Some(Reference(to)))

  private val lexerPacker: Packer[String, Char, Vector[ElodinToken]] =
    (P("(").as(Some(Parenthesis.Open)) |
      P(")").as(Some(Parenthesis.Close)) |
      P("fn").as(Some(Fn)) |
      fromPartial {
        case char if char.isWhitespace => null
      }.rep(min = 1)
        .as(None) |
      P("[").as(Some(Bracket.Open)) |
      P("]").as(Some(Bracket.Close)) |
      P("{").as(Some(Curly.Open)) |
      P("}").as(Some(Curly.Close)) |
      P("<:").as(Some(Yield)) |
      P("do").as(Some(Do)) |
      P("let").as(Some(Let)) |
      P(":").as(Some(Colon)) |
      refToken)
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
