package com.github.caeus.elodin.interpreter.printers

import com.github.caeus.elodin.interpreter.Val
import com.github.caeus.elodin.interpreter.Val.Lazy
import io.circe.Json

object ForVal {

  def print(value: Val): String =
    value match {
      case Lazy(v) =>
        s"(virtual:$v)"

      case _ => "not implemented"
    }

}
