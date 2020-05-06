package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.Val.{Atom, Lazy}
import com.github.caeus.elodin.interpreter.printers.ForVal
import com.github.caeus.elodin.interpreter.scope.Scope
import com.github.caeus.elodin.interpreter.scope.Scope.{
  WhenApply,
  WhenArr,
  WhenBool,
  WhenDict,
  WhenFloat,
  WhenFn,
  WhenInt,
  WhenLet,
  WhenRef,
  WhenText
}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.FnNode
import zio.Task

sealed trait Val extends Serializable {
  override final def toString: String = ForVal.print(this)
}
trait AFn extends Serializable {
  def take(value: Val): Task[Val]
}

object Prelude {
  final val identity: AFn = (value: Val) => Task.succeed(value)
}
case object Void extends Serializable

object Val {
  case class Atom(value: Serializable) extends Val
  case class Lazy(value: Scope[Node])  extends Val
}
case class BuiltIn(arity: Int, op: Seq[Val] => Task[Val])
case class Collector(
    marker: String,
    arity: Int,
    currentArgs: Vector[Val],
    done: Seq[Val] => Task[Val]
) extends AFn {
  override def take(value: Val): Task[Val] =
    if (arity - 1 == currentArgs.size) {
      done(currentArgs.appended(value))
    } else Task.succeed(Atom(Collector(marker, arity, currentArgs.appended(value), done)))
}
case class VirtualWhatever(scope: Scope[FnNode], currentArgs: Vector[Val]) extends AFn {
  val arity = scope.node.params.size
  override def take(value: Val): Task[Val] =
    if (arity - 1 == currentArgs.size) {
      val serializable = scope
        .body(currentArgs.appended(value))
        .map(sc => Task.succeed(Lazy(sc)))
        .getOrElse(Task.fail(new Exception("Incredible")))
      serializable
    } else Task.succeed(Atom(VirtualWhatever(scope, currentArgs.appended(value))))
}
case class WithRemnant(value: Val, remnant: Seq[Val])

class Interpreter(moduleLoader: FFI) {
  def run(node: Node): Task[Atom] = {
    eval(Scope.root(node))
  }

  def apply(args: List[Val]): Task[Atom] = {
    args match {
      case Nil => Task.succeed(Atom(Void))
      case _ =>
        args
          .foldLeft[Task[Val]](Task.succeed(Atom(Prelude.identity))) { (fn, arg) =>
            fn.flatMap(atomize).flatMap {
              case Atom(fn: AFn) =>
                fn.take(arg)
              case _ => Task.fail(new Exception("this cannot be applied, duh!"))
            }
          }
          .flatMap(atomize)
    }
  }

  def eval(scope: Scope[Node]): Task[Atom] = {
    scope match {
      case WhenApply(scope) =>
        apply(scope.argScopes.toList.map(_.toLazy))
      case WhenLet(scope) =>
        eval(scope.body)
      case WhenFn(scope) =>
        apply(Atom(VirtualWhatever(scope, Vector.empty)) :: Nil)
      case WhenRef(scope) =>
        scope
          .resolveRef(scope.node.to)
          .map(atomize)
          .getOrElse(Task.fail(new Exception("alkjdlaskjdlkasjjlaksdjklkl192837978123")))
      case WhenDict(scope) =>
        Task.succeed(
          Atom(scope.itemScopes.view.mapValues(_.toLazy).toMap.asInstanceOf[Serializable])
        )
      case WhenArr(scope) =>
        Task.succeed(Atom(scope.itemScopes.map(_.toLazy).asInstanceOf[Serializable]))
      case WhenText(scope) =>
        Task.succeed(Atom(scope.node.value))
      case WhenFloat(scope) =>
        Task.succeed(Atom(scope.node.value))
      case WhenInt(scope) =>
        Task.succeed(Atom(scope.node.value))
      case WhenBool(scope) =>
        Task.succeed(Atom(scope.node.value))
    }
  }

  def atomize(value: Val): Task[Atom] = {
    value match {
      case atom: Atom  => Task.succeed(atom)
      case Lazy(scope) => eval(scope)
    }
  }

}
