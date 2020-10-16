package io.github.caeus.elodin.core

case class EvalError(msg: String, parent: Option[EvalError]) {
  def toList: List[String] = msg :: parent.map(_.toList).getOrElse(Nil)
  override def toString: String = {
    toList.mkString("\n")
  }
}
