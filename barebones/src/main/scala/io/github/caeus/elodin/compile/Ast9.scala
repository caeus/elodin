package io.github.caeus.elodin.compile

import io.github.caeus.elodin.eval.Eval.Kernel
import io.github.caeus.elodin.eval._
import io.github.caeus.elodin.util.Pos2Loc.Loc
import io.github.caeus.elodin.value.Value.ImplRef
import io.github.caeus.elodin.value.{Eson, Value}
import zio.ZIO

sealed trait Ast9
object Ast9 {

  sealed trait Expr extends Ast9 {
    def loc: Loc
  }
  sealed trait FhunkRef
  object FhunkRef {
    final case class Guest(module: String, index: Int) extends FhunkRef
    final case class Host(name: String)                extends FhunkRef
  }
  object Expr {
    final case class Apply(fn: Expr, arg0: Expr, args: Seq[Expr], loc: Loc)           extends Expr
    final case class Extend(upon: Expr, body: Expr, loc: Loc)                         extends Expr
    final case class Fun(ref: Int, loc: Loc)                                          extends Expr
    final case class Req(module: String, loc: Loc)                                    extends Expr
    final case class Ref(to: String, loc: Loc)                                        extends Expr
    final case class Rec(bindings: Map[String, FhunkRef.Guest], body: Expr, loc: Loc) extends Expr
    final case class List(of: Seq[Expr], loc: Loc)                                    extends Expr
    final case class Dict(of: Map[String, Expr], loc: Loc)                            extends Expr
    final case class Literal(eson: Eson, loc: Loc)                                    extends Expr
    final case class HostRef(named: String, loc: Loc)                                 extends Expr
  }
  //Thunk + Function => Fhunk!
  final case class GuestFhunk(params: Vector[String], body: Expr) extends Ast9 {
    override def toString: String = params.mkString("[", ",", "]") ++ body.toString
  }
  final case class GuestModule(fhunks: Vector[GuestFhunk]) extends Ast9 {
    override def toString: String = "--------------\n" ++ fhunks.mkString("\n")
  }

  def eval(
      scope: Scope
  )(
      expr: Expr
  ): ZIO[Kernel, EvalException, Value] = {
    ZIO
      .environment[Kernel]
      .flatMap { kernel =>
        expr match {
          case Expr.Apply(fn, arg0, args, _) =>
            (eval(scope)(fn) <*> eval(scope)(arg0) <*> ZIO.collectAll(args.map { arg =>
              eval(scope)(arg)
            })).flatMap { (fn, arg0, args) =>
              Eval(fn, arg0, args)
            }

          case Expr.Extend(upon, body, _) =>
            eval(scope)(upon).flatMap {
              case Value.DictVal(value) =>
                eval(scope.extendWithValues(value))(body)
              case upn => EvalException.wildcardError(upn)
            }
          case Expr.Fun(index, _) =>
            ZIO.succeed(Value.FunVal(ImplRef.Guest(scope.module, index, scope), Nil))
          case Expr.Req(module, _) =>
            kernel.get[Archive].module(module)
          case Expr.Ref(to, _) =>
            scope
              .get(to)
              .map(v => ZIO.succeed(v))
              .getOrElse(EvalException.referenceError(to))
          case Expr.Rec(bindings: Map[String, FhunkRef.Guest], body, _) =>
            eval(scope.extendWithFhunkRefs(bindings))(body)
          case Expr.List(of, _) =>
            ZIO
              .collectAll(of.map { expr =>
                eval(scope)(expr)
              })
              .map(is => Value.ListVal(is))
          case Expr.Dict(of, _) =>
            ZIO
              .collectAll(of.map {
                case (key, v) => eval(scope)(v).map(key -> _)
              })
              .map(is => Value.DictVal(is.toMap))
          case Expr.Literal(eson, _) =>
            ZIO.succeed(Value.fromEson(eson))
          case Expr.HostRef(named, _) =>
            kernel.get[ENI].reduce(named, Nil)
        }
      }
      .catchAll(
        _.prependTrace(TraceElement.CodeLocation(file = scope.module, expr.loc.line, expr.loc.col))
      )
  }

}
