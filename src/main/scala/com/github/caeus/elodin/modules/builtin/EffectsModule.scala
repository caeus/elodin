package com.github.caeus.elodin.modules.builtin

import com.github.caeus.elodin.modules.ModuleContainer
import com.github.caeus.elodin.types.EloEffect._

object EffectsModule extends ModuleContainer {
  override def name: String = "effects"

  override def define = { b =>
    b("halt")(is[String] :: array :: isFn :: isFn) {
      case (id, args, onS, onF) => Halt(Desc(id, args), onS, onF).asAtom
    } ~
      b("done")(!!) { r =>
        Done(r).asAtom
      } ~
      b("failed")(!!) { r =>
        Failed(r).asAtom
      }
  }
}
