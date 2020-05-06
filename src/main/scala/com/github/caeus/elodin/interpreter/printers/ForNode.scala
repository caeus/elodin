package com.github.caeus.elodin.interpreter.printers

import com.github.caeus.elodin.frontend.ElodinToken
import com.github.caeus.elodin.frontend.ElodinToken._
import com.github.caeus.elodin.interpreter.printers.PrinterBuilder.IndentedPrinterBuilder
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import io.circe.Json

import scala.collection.mutable

trait PrinterBuilder {
  def add(token: ElodinToken*): Unit
  def dump: String
}
object PrinterBuilder {
  final class IndentedPrinterBuilder(maxColums: Int) extends PrinterBuilder {
    var indentation             = 0
    val whole: StringBuilder    = new mutable.StringBuilder()
    var currLine: StringBuilder = _
    newLine()
    private def newLine(): Unit = {
      if (currLine != null)
        whole.append("\n").append(currLine)
      currLine = new mutable.StringBuilder(" " * indentation)
    }
    private def tokenString(lastChar: Option[Char], token: ElodinToken): String = {

      val tsrt = token match {
        case Parenthesis.Open  => "("
        case Parenthesis.Close => ")"
        case Curly.Open        => "{"
        case Curly.Close       => "}"
        case Bracket.Open      => "["
        case Bracket.Close     => "]"
        case Text(value)       => Json.fromString(value).noSpaces
        case FloatNum(value)   => Json.fromBigDecimal(value).noSpaces
        case IntNum(value)     => Json.fromBigInt(value).noSpaces
        case Bool(value)       => value.toString
        case Colon             => ":"
        case Let               => "let"
        case Fn                => "fn"
        case Do                => "do"
        case Yield             => "<:"
        case Ignore            => "_"
        case Reference(to)     => to
        case _                 => ???
      }
      lastChar
        .map { char =>
          if (char.isLetter || tsrt.charAt(0).isLetter || char.isDigit || tsrt.charAt(0).isDigit) {
            s" $tsrt"
          } else tsrt
        }
        .getOrElse(tsrt)

    }
    private def addOne(token: ElodinToken): Unit = {
      if (currLine.size > maxColums) {
        newLine()
      }
      val str = tokenString(currLine.lastOption, token)

      currLine.append(str)
      token match {
        case Parenthesis.Open  => indentation = indentation + 1
        case Curly.Open        => indentation = indentation + 1
        case Bracket.Open      => indentation = indentation + 1
        case Parenthesis.Close => indentation = indentation - 1
        case Curly.Close       => indentation = indentation - 1
        case Bracket.Close     => indentation = indentation - 1
        case _                 =>
      }
    }
    override def add(token: ElodinToken*): Unit = token.foreach(addOne)

    override def dump: String = whole.toString()
  }
}

object ForNode {

  def depth(node: Node): Int = node match {
    case LetNode(bindings, body) =>
      1 + Math.max(bindings.values.map(depth).max, depth(body))
    case FnNode(params, body) =>
      1 + depth(body)
    case ArrNode(items) =>
      1 + items.map(depth).max
    case DictNode(items) =>
      1 + items.values.map(depth).max
    case ApplyNode(args) =>
      1 + args.map(depth).max
    case _ => 0
  }

  def toTokens(node: Node): List[ElodinToken] = {
    val builder = List.newBuilder[ElodinToken]
    addTokensTo(builder)(node)
    builder.result()
  }

  def print2(node: Node): String = ???

  private def addTokensTo(builder: mutable.Builder[ElodinToken, List[ElodinToken]]): Node => Unit = {
    case LetNode(bindings, body) =>
      builder ++= Seq(Parenthesis.Open, Let, Curly.Open)
      bindings.toSeq
        .foreach {
          case (binding, node) =>
            builder ++= Seq(Reference(binding), Colon)
            addTokensTo(builder)(node)
        }
      builder += Curly.Close
      addTokensTo(builder)(body)
      builder += Parenthesis.Close

    case FnNode(params, body) =>
      builder ++= Seq(Parenthesis.Open, Fn, Bracket.Open)
      params.foreach { param =>
        builder += Reference(param)
      }
      builder += Bracket.Close
      addTokensTo(builder)(body)
      builder += Parenthesis.Close
    case ApplyNode(args) =>
      builder += Parenthesis.Open
      args.foreach(addTokensTo(builder))
      builder += Parenthesis.Close
    case ArrNode(items) =>
      builder += Bracket.Open
      items.foreach(addTokensTo(builder))
      builder += Bracket.Close
    case DictNode(items) =>
      builder += Curly.Open
      items.toSeq
        .foreach {
          case (binding, node) =>
            builder ++= Seq(Reference(binding), Colon)
            addTokensTo(builder)(node)
        }
      builder += Curly.Close
    case TextNode(value) =>
      builder += Text(value)
    case FloatNode(value) =>
      builder += FloatNum(value)
    case IntNode(value) =>
      builder += IntNum(value)
    case BoolNode(value) =>
      builder += Bool(value)
    case RefNode(to) =>
      builder += Reference(to)
  }

  def print(node: Node): String = {
    val builder = new IndentedPrinterBuilder(40)
    builder.add(toTokens(node): _*)
    builder.dump
  }

}
