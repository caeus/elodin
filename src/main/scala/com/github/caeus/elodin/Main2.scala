package com.github.caeus.elodin

import com.github.caeus.elodin.interep.Swap.Native
import com.github.caeus.elodin.interep.{Cmd, Swap}
import com.github.caeus.elodin.runtime.Val.{PRef, App}
import com.github.caeus.elodin.runtime._
import zio._

object Main2 {

  val loader: SwapLoader = {
    case PRef("rt", "true") =>
      Task.succeed(Native(2, args => Task.effect(args(0))))
    case PRef("math", "add") =>
      Task.succeed(
        Native(
          arity = 2,
          reduce = args =>
            for {
              (ux, uy) <- Task.effect(args(0)).zipPar(Task.effect(args(1)))
              engine   <- RIO.environment[ExprEngine]
              nums     <- engine.atomize(ux).zipPar(engine.atomize(uy))
              result <- nums match {
                case (Val.Integer(x), Val.Integer(y)) =>
                  Task.succeed(Val.Integer(x.+(y)))
                case (Val.Real(x), Val.Real(y)) =>
                  Task.succeed(Val.Real(x.+(y)))
                case _ =>
                  Task.fail(new IllegalArgumentException("Wrong types"))
              }
            } yield result
        ))
    case PRef("main", "main") =>
      Task.succeed(Swap.Compiled(1, Seq(Cmd.PRef("main", "main"), Cmd.Arg(0), Cmd.Apply(2))))
    case pref =>
      Task.fail(new IllegalArgumentException(s"No definition found for program ref $pref"))
  }

  val engine: ExprEngine = new DefaultExprEngine(loader)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    engine
      .evalS(App(PRef("main", "main"), Seq(Val.App(PRef("main", "main"), Nil))))
      .fold({ e =>
        e.printStackTrace()
        1
      }, { x =>
        println(x)
        0
      })
  }
}
