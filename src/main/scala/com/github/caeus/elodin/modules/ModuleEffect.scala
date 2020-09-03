package com.github.caeus.elodin.modules

import com.github.caeus.elodin.runtime.Val
import com.github.caeus.elodin.runtime.Val.{Atomic, FnPointer}

sealed trait ModuleOp
object ModuleOp {
  case class DefMember(name: String, value: Val) extends ModuleOp
}
sealed trait ModuleEffect {}
object ModuleEffect {
  case class Halt(op: ModuleOp, cont: FnPointer) extends ModuleEffect
  case class Done(value: Atomic)                 extends ModuleEffect
}
