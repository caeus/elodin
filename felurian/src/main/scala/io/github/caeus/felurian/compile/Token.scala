package io.github.caeus.felurian.compile

import io.github.caeus.felurian.value.Value

sealed trait Token
object Token {
  sealed trait Delimiter { del =>
    case object Open extends Token {
      override def toString: String = s"$del.Open"
    }
    case object Close extends Token {
      override def toString: String = s"$del.Closed"
    }
  }
  final case object Curly                      extends Delimiter
  final case object Bracket                    extends Delimiter
  final case object Parenthesis                extends Delimiter
  final case class Val(value: Value)           extends Token
  final case object Do                         extends Token
  final case object Let                        extends Token
  final case object For                        extends Token
  final case object Fun                        extends Token
  final case object Module                     extends Token
  final case object Import                     extends Token
  final case object Comma                      extends Token
  final case object Semicolon                  extends Token
  final case object Colon                      extends Token
  final case object Arrow                      extends Token
  final case object Equals                     extends Token
  final case class Identifier(name: String)    extends Token
  final case object RefSep                     extends Token
  final case class Operator(name: String)      extends Token
  final case object Dot                        extends Token

}
