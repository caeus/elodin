package com.github.caeus.elodin.builtin

import com.github.caeus.elodin.interep.Swap
import com.github.caeus.elodin.interep.Swap.Native
import com.github.caeus.elodin.runtime.Gen.{Done, Halted}
import com.github.caeus.elodin.runtime.Val.{App, Eff, PRef}
import com.github.caeus.elodin.runtime.{ExprEngine, Val}
import zio.RIO

object StrictNative {
  def apply(arity: Int, cls: Seq[Val] => RIO[ExprEngine, Val]) =
    Native(arity, { vals: Seq[Val] =>
      for {
        engine <- RIO.environment[ExprEngine]
        vals   <- RIO.collectAllPar(vals.map(v => engine.atomize(v)))
        r      <- cls(vals)
      } yield r
    })
}

object Runtime {

  def appTo(app: App, args: Val*): App = {
    App(app.func, app.args.appendedAll(args))
  }
  val effects: String => Swap = {
    case "fail" =>
      StrictNative(1, {
        case Seq(value) =>
          RIO.succeed(Eff(Done(Left(value))))
      })
    case "succeed" =>
      StrictNative(1, {
        case Seq(value) =>
          RIO.succeed(Eff(Done(Right(value))))
      })
    case "in_chain" =>
      StrictNative(3, {
        case Seq(next: App, f: App, input: Val) =>
          RIO.succeed(App(PRef("effects", "chain"), Seq(appTo(next, input), f)))
      })
    case "chain" =>
      StrictNative(
        2, {
          case Seq(ma: Eff, fmb: App) =>
            ma.value match {
              case Halted(desc, ok, err) =>
                for {
                  engine <- RIO.environment[ExprEngine]
                  result <- RIO.succeed(App(PRef("effects", "in_chain"), Seq(ok, fmb)))
                } yield result
              case Done(Right(value)) =>
                for {
                  engine <- RIO.environment[ExprEngine]
                  result <- engine.atomize(appTo(fmb, value)).flatMap {
                    case eff: Eff => RIO.succeed(eff)
                    case _        => ???
                  }
                } yield result
              case Done(Left(value)) =>
                RIO.succeed(ma)
            }
        }
      )
  }
}
