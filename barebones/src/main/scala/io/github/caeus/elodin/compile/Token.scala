package io.github.caeus.elodin.compile

import io.github.caeus.elodin.value.Eson

sealed trait Token
object Token {
  final case class MetaToken[+M](token: Token, meta: M) {
    def map[T](f: M => T): MetaToken[T] = MetaToken(token, f(meta))
  }
  sealed trait Delimiter { del =>
    case object Open extends Token {
      override def toString: String = s"$del.Open"
    }
    case object Close extends Token {
      override def toString: String = s"$del.Closed"
    }
  }
  case object Curly                         extends Delimiter
  case object Bracket                       extends Delimiter
  case object Parenthesis                   extends Delimiter
  final case class Literal(value: Eson)     extends Token
  case object Do                            extends Token
  case object Let                           extends Token
  case object For                           extends Token
  case object Fun                           extends Token
  case object Module                        extends Token
  case object Import                        extends Token
  case object Comma                         extends Token
  case object Semicolon                     extends Token
  case object Colon                         extends Token
  case object Arrow                         extends Token
  case object Equals                        extends Token
  final case class Identifier(name: String) extends Token
  case object RefSep                        extends Token
  final case class Operator(name: String)   extends Token
  case object Dot                           extends Token

}
