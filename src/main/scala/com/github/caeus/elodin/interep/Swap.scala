package com.github.caeus.elodin.interep

import com.github.caeus.elodin.interep.Cmd.CmdExpr
import com.github.caeus.elodin.runtime.{ExprEngine, Val}
import zio.RIO


trait Swap {
  def arity: Int
}

object Swap {
  case class Compiled(arity: Int, ops: CmdExpr)                           extends Swap
  case class Native(arity: Int, reduce: Seq[Val] => RIO[ExprEngine, Val]) extends Swap
}

trait Cmd
object Cmd {
  type CmdExpr = Seq[Cmd]
  case class Real(value: BigDecimal)          extends Cmd
  case class Integer(value: BigInt)           extends Cmd
  case class Bool(value: Boolean)             extends Cmd
  case class Str(value: String)               extends Cmd
  case class PRef(module: String, id: String) extends Cmd
  case class Arg(index: Int)                  extends Cmd
  case class Apply(take: Int)                 extends Cmd
}