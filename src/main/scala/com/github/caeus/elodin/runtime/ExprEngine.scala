package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.interep.Swap.{Compiled, Native}
import com.github.caeus.elodin.interep.{Cmd, Swap}
import com.github.caeus.elodin.runtime.Val.{App, Eff, PRef}
import zio.{RIO, Task}

trait EffectRunner {

  def run: Unit
}
trait Gen {}
object Gen {
  case class Halted(desc: Val, ok: App, err: App) extends Gen
  case class Done(value: Either[Val, Val]) extends Gen
}

sealed trait Val

object Val {
  case class PRef(module: String, id: String)
  case class Str(value: String)              extends Val
  case class Real(value: BigDecimal)         extends Val
  case class Integer(value: BigInt)          extends Val
  case class Bool(value: Boolean)            extends Val
  case class Arr(value: Seq[Val])            extends Val
  case class Dict(value: Map[String, Val])   extends Val
  case class App(func: PRef, args: Seq[Val]) extends Val
  case class Eff(value: Gen)                 extends Val
}

trait ExprEngine {
  def buildS(value: Seq[Val]): Task[Val]
  def evalS(expr: App): Task[Val]
  final def atomize(value: Val): Task[Val] = {
    value match {
      case expr: App => evalS(expr)
      case _         => Task.succeed(value)
    }
  }
}
case class EvalDeps(stack: Stack[Val], func: Swap, args: Seq[Val])

final class DefaultExprEngine(loader: SwapLoader) extends ExprEngine {
  def buildS(value: Seq[Val]): Task[Val] = {
    Task.effect {
      value.toList match {
        case atom :: Nil =>
          atom
        case App(fref, iargs) :: oargs =>
          App(fref, iargs.toList ::: oargs)
        case Val.Bool(cond) :: ifargs =>
          App(PRef("rt", cond.toString), ifargs)
        case _ => ???
      }
    }
  }

  def toEffect(cmd: Cmd): RIO[EvalDeps, Unit] = {
    for {
      EvalDeps(stack: Stack[Val], func: Swap, args: Seq[Val]) <- RIO.environment[EvalDeps]
      _ <- cmd match {
        case Cmd.Real(value: BigDecimal) =>
          stack.push(Val.Real(value))
        case Cmd.Integer(value: BigInt) =>
          stack.push(Val.Integer(value))
        case Cmd.Bool(value: Boolean) =>
          stack.push(Val.Bool(value))
        case Cmd.Str(value: String) =>
          stack.push(Val.Str(value))
        case Cmd.PRef(module: String, id: String) =>
          stack.push(Val.App(PRef(module, id), Nil))
        case Cmd.Arg(index: Int) =>
          for {
            param <- RIO.effect(args(index))
            _     <- stack.push(param)
          } yield ()
        case Cmd.Apply(take: Int) =>
          for {
            data  <- stack.popMany(take)
            value <- buildS(data)
            _     <- stack.push(value)
          } yield ()
      }
    } yield ()
  }

  private def evalFunc(func: Swap, args: Seq[Val]): Task[Val] = {
    if (func.arity != args.size)
      Task.fail(new IllegalArgumentException("alkdaslkjd"))
    else
      func match {
        case code: Compiled =>
          for {
            stack <- Stack.make[Val]
            _ <- RIO
              .foreach_(code.ops)(toEffect)
              .provide(EvalDeps(stack, code, args))
            result <- stack.release
          } yield result
        case native: Native => native.reduce(args).provide(this)
      }
  }

  def evalS(apply: App): Task[Val] = {
    loader
      .get(apply.func)
      .flatMap {
        case func if func.arity > apply.args.size => Task.succeed(apply)
        case func =>
          for {
            res0   <- evalFunc(func, apply.args.take(func.arity))
            result <- evalS(apply.args.drop(func.arity).prepended(res0))
          } yield result
      }
  }

  def evalS(values: Seq[Val]): Task[Val] = {
    for {
      value <- buildS(values.toList)
      result <- value match {
        case sexpr: App => evalS(sexpr)
        case data       => Task.succeed(data)
      }
    } yield result
  }
}

trait SwapLoader {
  def get(fref: PRef): Task[Swap]
}

trait EffectResolver {
  def get(id: String): Task[Eff]
}
