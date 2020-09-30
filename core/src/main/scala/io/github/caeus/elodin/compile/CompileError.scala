package io.github.caeus.elodin.compile

case class CompileError(msg: String, parent: Option[CompileError])
