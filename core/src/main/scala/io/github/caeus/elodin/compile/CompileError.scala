package io.github.caeus.elodin.compile

final case class LocalizedError(path: Vector[String], msg: String)
sealed trait CompileError

object CompileError {
  case class AssemblingError(of: Seq[LocalizedError], parent: Option[CompileError])
      extends CompileError
  case class ParsingError(msg: String, parent: Option[CompileError]) extends CompileError
}
