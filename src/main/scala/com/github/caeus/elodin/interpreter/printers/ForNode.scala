package com.github.caeus.elodin.interpreter.printers

import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.{
  ApplyNode,
  ArrNode,
  BoolNode,
  DictNode,
  FloatNode,
  IntNode,
  FnNode,
  LetNode,
  RefNode,
  ReqNode,
  TextNode
}
import io.circe.Json

object ForNode {
  def print(nesting: Int): Node => String = {
    case LetNode(bindings, body) =>
      bindings.toSeq
        .map {
          case (key, value) => s"$key= ${print(nesting + 1)(value)}"
        }
        .mkString("let {", ", ", s"} in ${print(nesting + 1)(body)}")
    case FnNode(params, body) =>
      s"""fun(${params.mkString(", ")}) =
         |${"  " * nesting}${print(nesting + 1)(body)}""".stripMargin
    case TextNode(value)                 => Json.fromString(value).noSpaces
    case ApplyNode(args) if args.isEmpty => "Unit"
    case ApplyNode(args) =>
      args.map(ForNode.print(nesting + 1)).mkString("(", ", ", ")")
    case IntNode(value)   => value.toString
    case FloatNode(value) => value.toString()
    case BoolNode(value)  => value.toString
    case ArrNode(items)   => items.map(ForNode.print(nesting + 1)).mkString("[", ", ", "]")
    case DictNode(items) =>
      items.toSeq
        .map {
          case (key, value) => s"$key: ${print(nesting + 1)(value)}"
        }
        .mkString("{", ", ", "}")
    case ReqNode(to) => s"require(${Json.fromString(to).noSpaces})"
    case RefNode(to) => to
  }
}
