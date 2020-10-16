package io.github.caeus.elodin.compile

final case class LocalizedError(path: Vector[String], msg: String)
final case class CompileError(of: Seq[LocalizedError], parent: Option[CompileError])
