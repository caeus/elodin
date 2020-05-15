package com.github.caeus.elodin.modules.builtin

import com.github.caeus.elodin.modules.ModuleContainer
import com.github.caeus.elodin.types.ModuleEffect._
import com.github.caeus.elodin.types.ModuleOp

object ModulesModule extends ModuleContainer {
  override def name: String = "effects"

  override def define = { b =>
    b("halt")(is[ModuleOp] :: isFn) {
      case (op, onS) => Halt(op, onS).asAtom
    } ~
      b("done")(!!) { r =>
        Done(r).asAtom
      } ~ b("def")(is[String] :: !!) {
      case (name, value) =>
        Halt(ModuleOp.DefMember(name, value), pointer("done")).asAtom
    }
  }
}
