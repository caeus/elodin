package com.github.caeus.elodin.compile

import com.github.caeus.elodin.interep.Cmd.CmdExpr
import com.github.caeus.elodin.interep.{Cmd, Swap}
import com.github.caeus.elodin.lang.Node._
import com.github.caeus.elodin.lang.Path
import zio.Task

case class SwapHeader(params: Seq[DeclParam]) {
  def indexOf(declParam: DeclParam): Task[Int] = {
    params.indexOf(declParam) match {
      case num if num < 0 => Task.fail(new Exception("alkjdslaksjd"))
      case num            => Task.succeed(num)
    }
  }
}
class Node2Swap(moduleName: String, root: CtxNode, table: Map[Path, Set[DeclParam]]) {

  def headerPrefixFor(path: Path): Task[Seq[DeclParam]] = {
    for {
      captured <- Task.effect(table(path))
    } yield captured.toSeq.sortBy(p => p.path.toString -> p.index)
  }

  def invocationCmds(path: Path, enclosing: SwapHeader): Task[Seq[Cmd]] = {
    headerPrefixFor(path)
      .flatMap { params =>
        Task.collectAll(params.map(enclosing.indexOf))
      }
      .map(_.map(Cmd.Arg).reverse)
      .map { params: Seq[Cmd] =>
        params.appendedAll(Seq(Cmd.PRef(moduleName, path.toString), Cmd.Apply(params.size + 1)))
      }
  }

  def headerOf(ctxNode: CtxNode): Task[SwapHeader] = {
    for {
      headerPrefix <- headerPrefixFor(ctxNode.path)
      headerPosfix: Seq[DeclParam] = ctxNode.node match {
        case lambdaNode: LambdaNode =>
          (0 until lambdaNode.params.size).map { index =>
            DeclParam(ctxNode.path, index)
          }
        case _ => Seq[DeclParam]()
      }
    } yield SwapHeader(headerPrefix.appendedAll(headerPosfix))
  }

  def toSwapBody(ctxNode: CtxNode, header: SwapHeader): Task[CmdExpr] = {
//    ctxNode.node match {
//      case moduleRefNode: RequireNode =>
//        ???
//      case letNode: LetNode =>
//        ctxNode.down.flatMap(ctx => toSwapBody(ctx, header))
//      case lambdaNode: LambdaNode =>
//        ctxNode.down.flatMap(ctx => toSwapBody(ctx, header))
//      case refNode: RefNode =>
//        ctxNode.resolveRef(refNode.to) {
//          case IsNode(ctx) =>
//            invocationCmds(ctx.path, header)
//          case IsParam(ctx, index) =>
//            header.indexOf(DeclParam(ctx.path, index)).map { index =>
//              Seq(Cmd.Arg(index))
//            }
//          case IsExternal(module, id) =>
//            Task.succeed(Seq(Cmd.PRef(module, id)))
//        }
//      case appNode: AppNode =>
//        for {
//          contexts <- ctxNode.manyIndexes(appNode.args.size).map(_.reverse)
//          cmds     <- Task.sequence(contexts.map(ct => toSwapBody(ct, header)))
//        } yield cmds.flatten.appended(Cmd.Apply(appNode.args.size))
//
//      case strNode: StrNode   => Task.succeed(Seq(Cmd.Str(strNode.value)))
//      case intNode: IntNode   => Task.succeed(Seq(Cmd.Integer(intNode.value)))
//      case realNode: RealNode => Task.succeed(Seq(Cmd.Real(realNode.value)))
//      case boolNode: BoolNode => Task.succeed(Seq(Cmd.Bool(boolNode.value)))
//      case dictNode: DictNode =>
//        for {
//          contexts <- ctxNode.manyKeys(dictNode.items.keySet)
//          cmds: Seq[Seq[Cmd]] <- Task.sequence(contexts.toSeq.map {
//            case (key, ctx) =>
//              toSwapBody(ctx, header).map { cmd =>
//                cmd.appended(Cmd.Str(key))
//              }
//          })
//        } yield
//          cmds.flatten.appendedAll(Seq(Cmd.Integer(cmds.size), Cmd.PRef(module = "dict", "make")))
//      case arrNode: ArrNode =>
//        for {
//          contexts <- ctxNode.manyIndexes(arrNode.items.size)
//          cmds: Seq[Seq[Cmd]] <- Task.sequence(contexts.map { ctx =>
//            toSwapBody(ctx, header)
//          })
//        } yield
//          cmds.flatten.appendedAll(Seq(Cmd.Integer(cmds.size), Cmd.PRef(module = "arr", "make")))
//    }
    ???
  }

  def toSwap(ctxNode: CtxNode): Task[Swap] = {
    for {
      header <- headerOf(ctxNode)
      body   <- toSwapBody(ctxNode, header)
    } yield Swap.Compiled(header.params.size, body)
  }

  def toSwap(path: Path): Task[Swap] = {
    for {
      params <- Task.effect(table(path))
      ctx    <- root.nested(path)
      swap   <- toSwap(ctx)
    } yield swap
  }

  def run(): Task[Map[Path, Swap]] = {
    Task
      .collectAll(table.keySet.toSeq.map { key =>
        toSwap(key).map(key -> _)
      })
      .map(_.toMap)
  }

}
