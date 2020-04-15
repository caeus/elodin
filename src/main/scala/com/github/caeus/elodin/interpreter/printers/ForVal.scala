package com.github.caeus.elodin.interpreter.printers

import com.github.caeus.elodin.interpreter.Val
import com.github.caeus.elodin.interpreter.Val.Lazy
import io.circe.Json

object ForVal {

  def print(value: Val): String = value match {
    case Lazy(impl) =>
      impl.fold(n => s"(native:${n.name}${n.args.map(x=>s" $x").mkString})", v => s"(virtual:$v)")
    case Val.Text(value)  => Json.fromString(value).noSpaces
    case Val.Int(value)   => value.toString
    case Val.Float(value) => value.toString
    case Val.Bool(value)  => value.toString
    case Val.Arr(items)   => items.mkString("[", ", ", "]")
    case Val.Dict(items) =>
      items.toSeq
        .map {
          case (key, value) => s"$key: ${value}"
        }
        .mkString("{", ", ", "}")
    case Val.Unit => "Unit"
  }

}
