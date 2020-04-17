package com.github.caeus.elodin.interpreter.printers

import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import io.circe.Json

object ForNode {
  def spaces(nesting: Int): String = " " * nesting
  def print(nesting: Int): Node => String = {
    case LetNode(bindings, body) =>
      bindings.toSeq
        .map {
          case (key, value) => s"${spaces(nesting + 1)}$key : ${print(nesting + 1)(value)}"
        }
        .mkString("(let {\n", "\n", s"\n${spaces(nesting)}} ${print(nesting + 1)(body)})")
    case FnNode(params, body) =>
      s"""(fn [${params.mkString(" ")}] ${print(nesting + 1)(body)})""".stripMargin
    case TextNode(value)                 => Json.fromString(value).noSpaces
    case ApplyNode(args) if args.isEmpty => "()"
    case ApplyNode(args) =>
      args
        .map { item =>
          s"${spaces(nesting + 1)}${ForNode.print(nesting + 1)(item)}"
        }
        .mkString("(\n", "\n", s"\n${spaces(nesting)})")
    case IntNode(value)   => value.toString
    case FloatNode(value) => value.toString()
    case BoolNode(value)  => value.toString
    case ArrNode(items) =>
      items
        .map { item =>
          s"${spaces(nesting + 1)}${ForNode.print(nesting + 1)(item)}"
        }
        .mkString("[\n", "\n", s"\n${spaces(nesting)}]")
    case DictNode(items) =>
      items.toSeq
        .map {
          case (key, value) => s"${spaces(nesting + 1)}$key : ${print(nesting + 1)(value)}"
        }
        .mkString("{\n", "\n", s"\n${spaces(nesting)}}")
    case ReqNode(to) => s"(require ${Json.fromString(to).noSpaces})"
    case RefNode(to) => to
  }
}
