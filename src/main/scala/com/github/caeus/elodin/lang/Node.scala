package com.github.caeus.elodin.lang

import com.github.caeus.elodin.interpreter.printers.ForNode
import com.github.caeus.elodin.lang.Step.{Down, Index, Key}

case class Meta[+A](value: A, children: List[Meta[A]])

sealed trait Step
object Step {
  case object Down extends Step {
    override def toString: String = "_"
  }
  case class Index(value: Int) extends Step {
    override def toString: String = value.toString
  }
  case class Key(value: String) extends Step {
    override def toString: String = value
  }
}

case class Path(value: Seq[Step]) {
  final def key(value: String): Path = /(Step.Key(value))
  final def index(value: Int): Path  = /(Step.Index(value))
  final def down: Path               = /(Step.Down)
  final def /(step: Step): Path      = Path(value.appended(step))
  final def parent                   = Path(value.init)
  final def head: Step               = value.head
  final def tail: Path               = Path(value.tail)
  override def toString              = "\\" + value.mkString(".")

}
object Path {
  def root = Path(Vector.empty)

}
sealed trait Node {
  override final def toString: String = ForNode.print(0)(this)
}

object Node {
  implicit class Ops(private val node: Node) extends AnyVal {
    def prettyPrint(nesting: Int): String = pretty(nesting)(node)
    def /(step: Step): Node = (step, node) match {
      case (Down, FnNode(_, body))          => body
      case (Down, LetNode(_, body))         => body
      case (Index(index), ApplyNode(args))  => args(index)
      case (Index(index), ArrNode(items))   => items(index)
      case (Key(key), LetNode(bindings, _)) => bindings(key)
      case (Key(key), DictNode(items))      => items(key)
      case _ =>
        throw new Exception(
          s"Step: $step is not valid for node of type: ${node.getClass.getSimpleName}")
    }
  }

  case class LetNode(bindings: Map[String, Node], body: Node) extends Node

  case class FnNode(
      params: Seq[String],
      body: Node
  ) extends Node

  case class ApplyNode(args: Seq[Node])         extends Node
  case class TextNode(value: String)            extends Node
  case class IntNode(value: BigInt)             extends Node
  case class FloatNode(value: BigDecimal)       extends Node
  case class BoolNode(value: Boolean)           extends Node
  case class ArrNode(items: Seq[Node])          extends Node
  case class DictNode(items: Map[String, Node]) extends Node
  case class RefNode(to: String)                extends Node
  case class ReqNode(to: String)                extends Node

  def pretty(nesting: Int)(node: Node): String =
    node match {
      case LetNode(bindings: Map[String, Node], body: Node) =>
        s"""
           |let {
           | ${bindings.view
             .mapValues(pretty(nesting + 1))
             .map {
               case (name, expr) => (" " * nesting) + s"$name = $expr"
             }
             .mkString(",\n")}
           |} in ${pretty(nesting + 1)(body)}
       """.stripMargin
      case FnNode(
          params: Seq[String],
          body: Node
          ) =>
        s"""{(${params.mkString(", ")})=>
           |${" " * nesting}${pretty(nesting + 1)(body)}}""".stripMargin
      case ApplyNode(args) =>
        s"(${args.map(pretty(nesting + 1)).mkString(" ")})"
      case TextNode(value: String) => s""""$value""""
      case IntNode(value)          => value.toString
      case FloatNode(value)        => value.toString
      case BoolNode(value)         => value.toString
      case ArrNode(items)          => s"[${items.map(pretty(nesting + 1)).mkString(", ")}]"
      case DictNode(items) =>
        s"""
           |{
           | ${items.view
             .mapValues(pretty(nesting + 1))
             .map {
               case (name, expr) => (" " * nesting) + s"$name = $expr"
             }
             .mkString(",\n")}
           |}
       """.stripMargin
      case RefNode(to) => to
      case ReqNode(to) => s"require($to)"
    }

}
