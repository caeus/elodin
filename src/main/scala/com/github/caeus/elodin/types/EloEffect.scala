package com.github.caeus.elodin.types

import com.github.caeus.elodin.runtime.Val

sealed trait EloEffect
object EloEffect {
  case class EffectDesc(id: String, args: Seq[Val])
  case class Halt(desc: EffectDesc, onSuccess: Val.Fn, onFailure: Val.Fn) extends EloEffect
  case class Done(value: Val.Atomic)                                      extends EloEffect
  case class Failed(value: Val.Atomic)                                    extends EloEffect
}
