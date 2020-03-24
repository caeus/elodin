package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.RefResolution.{IsNode, IsParam, Undefined}
import com.github.caeus.elodin.interpreter.Scope.Root
import com.github.caeus.elodin.interpreter.Val.App
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.{
  AppNode,
  ArrNode,
  BoolNode,
  DictNode,
  IntNode,
  LambdaNode,
  LetNode,
  RealNode,
  RefNode,
  ReqNode,
  StrNode
}
import zio.{RIO, Task}

sealed trait Val
object Val {
  implicit class ValOps(private val value: Val) extends AnyVal {
    def applyTo(args: Val*): Val = {
      value match {
        case App(code) => App(code.applyTo(args: _*))
        case _ =>
          throw new Exception(
            s"Value of type ${value.getClass.getSimpleName} cannot be applied to anything")
      }
    }
  }
  case class Str(value: String)            extends Val
  case class Real(value: BigDecimal)       extends Val
  case class Integer(value: BigInt)        extends Val
  case class Bool(value: Boolean)          extends Val
  case class Arr(value: Seq[Val])          extends Val
  case class Dict(value: Map[String, Val]) extends Val
  case class App(code: Impl)               extends Val
}

trait ModuleLoader {
  def get(name: String): Task[Val]
  def nativeArity(name: String): Task[(Int, Seq[Val] => RIO[Interpreter, Val])]
}
class Interpreter(moduleLoader: ModuleLoader) {

  def walkDown: Scope => Task[Val] = {
    case NodeOf(point, appNode: AppNode) =>
      for {
        points <- Task.effect(point.manyIndexes(appNode.args.size))
        args   <- Task.collectAll(points.map(walkDown))
        result <- Task.effect(args.head.applyTo(args.tail: _*))
      } yield result
    case NodeOf(scope, _: LetNode) =>
      walkDown(scope.down)
    case NodeOf(env, _: LambdaNode) =>
      Task.succeed(App(Impl.Virtual(env)))
    case NodeOf(env, refNode: RefNode) =>
      env.resolveRef(refNode.to) match {
        case IsParam(value) =>
          Task.succeed(value)
        case IsNode(refd) => Task.succeed(App(Impl.Virtual(refd)))
        case Undefined    => Task.fail(new Exception("qwoieuqwoieu"))
      }
    case NodeOf(point, dictNode: DictNode) =>
      Task
        .effect(point.manyKeys(dictNode.items.keySet).toSeq.map {
          case (key, asd) => walkDown(asd).map(key -> _)
        })
        .flatMap(Task.collectAll)
        .map(_.toMap)
        .map(Val.Dict)
    case NodeOf(point, arrNode: ArrNode) =>
      Task
        .effect(point.manyIndexes(arrNode.items.size).map(walkDown))
        .flatMap(Task.collectAll)
        .map(Val.Arr)
    case NodeOf(_, strNode: StrNode)   => Task.succeed(Val.Str(strNode.value))
    case NodeOf(_, intNode: IntNode)   => Task.succeed(Val.Integer(intNode.value))
    case NodeOf(_, realNode: RealNode) => Task.succeed(Val.Real(realNode.value))
    case NodeOf(_, boolNode: BoolNode) => Task.succeed(Val.Bool(boolNode.value))
    case NodeOf(_, requireNode: ReqNode) =>
      moduleLoader.get(requireNode.to)
    case NodeOf(_, x) =>
      println(x)
      ???
  }

  def run(module: String, node: Node): Task[Val] = walkDown(Root(module, node)).flatMap(reduce)
  def reduce(value: Val): Task[Val] = {
    value match {
      case ArgsOf.Native(name, args) =>
        moduleLoader.nativeArity(name).flatMap {
          case (arity, reducer) if args.size == arity =>
            reducer(args).provide(this).flatMap(reduce)
          case (arity, _) if arity > args.size =>
            Task.succeed(value)
          case (arity, reducer) =>
            reducer(args.take(arity))
              .provide(this)
              .flatMap { value =>
                Task.effect(value.applyTo(args.drop(arity): _*))
              }
              .flatMap(reduce)
        }
      case ArgsOf.Virtual(scope, args: Seq[Val]) =>
        scope.node match {
          case LambdaNode(params, body) =>
            val arity = params.size
            if (arity > args.size) {
              Task.succeed(value)
            } else {
              walkDown(scope.down)
                .flatMap { value =>
                  Task.effect(value.applyTo(args.drop(arity): _*))
                }
                .flatMap(reduce)
            }
          case _ =>
            walkDown(scope)
              .flatMap { value =>
                Task.effect(value.applyTo(args: _*))
              }
              .flatMap(reduce)

        }
      case _ => Task.succeed(value)
    }
  }
}
