package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.Val.{Atom, Lazy, Partial}
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
  WhenReq,
  WhenText
}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node.FnNode
import zio.Task

sealed trait Val {
  override final def toString: String = ForVal.print(this)
}

object Val {
  case class Native(name: String, arity: Int)

  sealed trait Atom                        extends Val
  case class Text(value: String)           extends Atom
  case class Float(value: BigDecimal)      extends Atom
  case class Integer(value: BigInt)        extends Atom
  case class Bool(value: Boolean)          extends Atom
  case class Arr(value: Seq[Val])          extends Atom
  case class Dict(value: Map[String, Val]) extends Atom
  case class Partial(impl: Either[Native, Scope[FnNode]], args: Seq[Val]) extends Atom {
    def arity: Int = impl.fold(_.arity, _.node.params.size) - args.size
  }
  case object Unit                    extends Atom
  case class Lazy(value: Scope[Node]) extends Val
}

case class WithRemnant(value: Val, remnant: Seq[Val])

class Interpreter(moduleLoader: ModuleLoader) {
  def run(node: Node): Task[Atom] = {
    eval(Scope.root(node))
  }
  def calcNative(args: Seq[Val])(impl: Val.Native): Task[Atom] = {
    ???
  }
  def calcFn(args: Seq[Val])(scope: Scope[FnNode]): Task[Atom] = {
    scope.body(args).map(eval).getOrElse(Task.fail(new Exception("lasdlaksjd")))
  }
  def calc(impl: Either[Val.Native, Scope[FnNode]], args: Seq[Val]): Task[Atom] = {
    impl.fold(calcNative(args), calcFn(args))
  }

  def apply(args: List[Val]): Task[Atom] = {
    args match {
      case Nil          => Task.succeed(Val.Unit)
      case value :: Nil => atomize(value)
      case (partial @ Partial(impl, prefixArgs)) :: args =>
        val (taken, remnant) = args.splitAt(partial.arity)
        calc(impl, prefixArgs.appendedAll(taken)).flatMap { atom =>
          apply(atom :: remnant)
        }

      case _ => Task.fail(new Exception("Wronglaksjdlaksjdlkas"))
    }
  }

  def eval(scope: Scope[Node]): Task[Atom] = {
    scope match {
      case WhenApply(scope) =>
        apply(scope.argScopes.toList.map(_.toLazy))
      case WhenLet(scope) =>
        eval(scope.body)
      case WhenFn(scope) =>
        apply(Partial(Right(scope), Nil) :: Nil)
      case WhenRef(scope) =>
        scope
          .resolveRef(scope.node.to)
          .map(atomize)
          .getOrElse(Task.fail(new Exception("alkjdlaskjdlkasjjlaksdjklkl192837978123")))
      case WhenReq(scope) =>
        moduleLoader.get(scope.node.to).flatMap(atomize)
      case WhenDict(scope) =>
        Task.succeed(Val.Dict(scope.itemScopes.view.mapValues(_.toLazy).toMap))
      case WhenArr(scope) =>
        Task.succeed(Val.Arr(scope.itemScopes.map(_.toLazy)))
      case WhenText(scope) =>
        Task.succeed(Val.Text(scope.node.value))
      case WhenFloat(scope) =>
        Task.succeed(Val.Float(scope.node.value))
      case WhenInt(scope) =>
        Task.succeed(Val.Integer(scope.node.value))
      case WhenBool(scope) =>
        Task.succeed(Val.Bool(scope.node.value))
    }
  }

  def atomize(value: Val): Task[Atom] = {
    value match {
      case atom: Atom  => Task.succeed(atom)
      case Lazy(scope) => eval(scope)
    }
  }

}
