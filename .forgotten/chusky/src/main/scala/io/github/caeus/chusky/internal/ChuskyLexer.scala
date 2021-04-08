package io.github.caeus.chusky.internal

import com.jsoniter.Jsoniter
import io.github.caeus.chusky.internal.ChuskyToken._
import io.github.caeus.plutus.PackerSyntax.StringPackerSyntax
import io.github.caeus.plutus.{Packer, PrettyPacker}

sealed trait ChuskyToken
object ChuskyToken {
  sealed trait Delimiter { del =>
    case object Open extends ChuskyToken {
      override def toString: String = s"$del.Open"
    }
    case object Close extends ChuskyToken {
      override def toString: String = s"$del.Closed"
    }
  }
  case object Star                 extends ChuskyToken
  final case class Name(v: String) extends ChuskyToken
  final case object Alias          extends ChuskyToken
  final case object Foreign        extends ChuskyToken
  final case object Colon          extends ChuskyToken
  final case object Semicolon      extends ChuskyToken
  final case object Comma          extends ChuskyToken
  final case object Arrow          extends ChuskyToken
  final case object Equals         extends ChuskyToken
  final case object Parenthesis    extends Delimiter
  final case object Curly          extends Delimiter
  final case object Bracket        extends Delimiter
  final case class Text(v: String) extends ChuskyToken
  final case object Import         extends ChuskyToken
  final case object Dot            extends ChuskyToken
  final case object Underscore     extends ChuskyToken
  final case object RPC            extends ChuskyToken
  final case object QMark          extends ChuskyToken
  final case object Union          extends ChuskyToken
  final case object Product        extends ChuskyToken
}

trait ChuskyLexer {
  def lex(source: String): Either[PrettyPacker.PackerException, Vector[ChuskyToken]]
}
final class DefaultChuskyLexer extends ChuskyLexer {
  import StringPackerSyntax._

  lazy val star = P("*").as(Star)
  lazy val name =
    (P(_.isLetter) ~ P(c => c.isLetterOrDigit || c == '$' || c == '_').rep()).!.map(_.value)
      .map(Name.apply)
  lazy val alias        = P("alias").as(Alias)
  lazy val foreign      = P("foreign").as(Foreign)
  lazy val union        = P("union").as(Union)
  lazy val product      = P("union").as(Product)
  lazy val rpc          = P("rpc").as(RPC)
  lazy val underscore   = P("_").as(Underscore)
  lazy val colon        = P(":").as(Colon)
  lazy val semicolon    = P(";").as(Semicolon)
  lazy val qmark        = P("?").as(QMark)
  lazy val comma        = P(",").as(Comma)
  lazy val arrowToken   = P("=>").as(Arrow)
  lazy val equalsToken  = P("=").as(Equals)
  lazy val parenOpen    = P("(").as(Parenthesis.Open)
  lazy val parenClose   = P(")").as(Parenthesis.Close)
  lazy val curlyOpen    = P("{").as(Curly.Open)
  lazy val curlyClose   = P("}").as(Curly.Close)
  lazy val bracketOpen  = P("[").as(Bracket.Open)
  lazy val bracketClose = P("]").as(Bracket.Close)
  lazy val importToken  = P("import").as(Import)

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

  lazy val anyToken = (P("""\s+""".r).as(None) | (
    star |
      alias |
      foreign |
      union |
      product |
      rpc |
      equalsToken |
      underscore |
      colon |
      comma |
      semicolon |
      qmark |
      arrowToken |
      parenOpen |
      parenClose |
      curlyOpen |
      curlyClose |
      bracketOpen |
      bracketClose |
      importToken |
      textToken |
      name
  ).asSome).rep.map(_.collect {
    case Some(token) => token
  })

  lazy val prettyPacker = PrettyPacker.version1(anyToken ~ End)

  def lex(source: String): Either[PrettyPacker.PackerException, Vector[ChuskyToken]] =
    prettyPacker.process(source)
}
