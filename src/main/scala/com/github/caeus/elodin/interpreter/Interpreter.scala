package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.Val.Lazy
import com.github.caeus.elodin.interpreter.printers.ForVal
import com.github.caeus.elodin.interpreter.scope.Scope
import com.github.caeus.elodin.interpreter.scope.Scope.{
  WhenApply,
  WhenArr,
  WhenBool,
  WhenDict,
  WhenFloat,
  WhenInt,
  WhenLambda,
  WhenLet,
  WhenRef,
  WhenReq,
  WhenStr
}
import com.github.caeus.elodin.lang.Node
import zio.Task

sealed trait Val {
  override final def toString: String = ForVal.print(this)
}

object Val {
  case class Native(name: String, args: Seq[Val]) {
    def applyTo(args: Seq[Val]) = Native(name, this.args.appendedAll(args))
  }

  case class Text(value: String)           extends Val
  case class Float(value: BigDecimal)      extends Val
  case class Int(value: BigInt)            extends Val
  case class Bool(value: Boolean)          extends Val
  case class Arr(value: Seq[Val])          extends Val
  case class Dict(value: Map[String, Val]) extends Val
  case class Lazy(impl: Either[Native, Scope[Node]]) extends Val {
    def applyTo(args: Seq[Val]) = Lazy(impl.map(_.applyTo(args)).left.map(_.applyTo(args)))
  }
  case object Unit extends Val
}

case class WithRemnant(value: Val, remnant: Seq[Val])

class Interpreter(moduleLoader: ModuleLoader) {

  def lazyApply(func: Val, args: Seq[Val]): Task[Val] =
    if (args.nonEmpty) func match {
      case Lazy(impl) =>
        Task.succeed(Lazy(impl.map(_.applyTo(args)).left.map(_.applyTo(args))))
      case _ => ???
    } else Task.succeed(func)

  def toVal: Scope[Node] => Task[Val] = {
    case WhenLet(scope) => toVal(scope.body)
    case WhenLambda(scope) =>
      scope.body
        .map(
          toVal
        )
        .getOrElse(Task.succeed(Val.Lazy(Right(scope.widen))))
    case WhenApply(scope) =>
      Task
        .collectAll(scope.argScopes.map(toVal))
        .flatMap {
          case func :: args => lazyApply(func, args)
          case Nil          => Task.succeed(Val.Unit)
        }
    case WhenArr(scope) =>
      Task.collectAll(scope.itemScopes.map(toVal)).map(Val.Arr.apply)
    case WhenDict(scope) =>
      Task
        .collectAll(scope.itemScopes.toSeq.map {
          case (key, kscope) =>
            toVal(kscope).map(key -> _)
        })
        .map { vals =>
          Val.Dict(vals.toMap)
        }
    case WhenRef(scope) =>
      scope
        .resolveRef(scope.node.to)
        .map {
          case Left(bindedScope) =>
            Task.succeed(Val.Lazy(Right(bindedScope)))
          case Right(value) =>
            Task.succeed(value)
        }
        .getOrElse(Task.fail(new Exception(";alskd;alksd;laskd")))
    case WhenReq(scope) =>
      moduleLoader.get(scope.node.to)
    case WhenStr(scope) =>
      Task
        .succeed(Val.Text(scope.node.value))
    case WhenFloat(scope) =>
      Task
        .succeed(Val.Float(scope.node.value))
    case WhenInt(scope) =>
      Task.succeed(Val.Int(scope.node.value))
    case WhenBool(scope) => Task.succeed(Val.Bool(scope.node.value))
  }

  def run(module: String, node: Node): Task[Val] = toVal(Scope.root(node)).flatMap(reduce)

  def reduce(scopeK: Scope[Node]): Task[WithRemnant] = {
    val arityAndArgs: Scope[Node] => Task[(Int, Seq[Val])] = {
      case WhenLambda(scope) => Task.succeed(scope.node.params.size -> scope.args.getOrElse(Nil))
      case scope             => Task.succeed(0                      -> scope.args.getOrElse(Nil))
    }
    arityAndArgs(scopeK).flatMap {
      case (arity, args) if args.size >= arity =>
        toVal(scopeK)
          .flatMap(reduce)
          .map(WithRemnant(_, args.drop(arity)))
      case (arity, args) if args.size < arity =>
        Task.succeed(WithRemnant(Lazy(Right(scopeK)), Nil))
    }
  }
  def reduce(native: Val.Native): Task[WithRemnant] = {
    val args = native.args
    moduleLoader.nativeImpl(native.name).flatMap {
      case NativeImpl(arity, reducer) if args.size >= arity =>
        reducer(args.take(arity))
          .provide(this)
          .flatMap(reduce)
          .map(WithRemnant(_, args.drop(arity)))
      case NativeImpl(arity, reducer) if args.size < arity =>
        Task.succeed(WithRemnant(Lazy(Left(native)), Nil))
    }
  }
  def reduce(value: Val): Task[Val] = {
    value match {
      case Val.Lazy(impl) =>
        impl
          .map(reduce)
          .left
          .map(reduce)
          .fold(identity, identity)
          .flatMap {
            case WithRemnant(resultValue, Nil) =>
              Task.succeed(resultValue)
            case WithRemnant(resultValue, remnant) =>
              lazyApply(resultValue, remnant)
                .flatMap(reduce)
          }
      case _ => Task.succeed(value)
    }
  }
}
