package com.github.caeus.elodin.compile

import com.github.caeus.elodin.lang.Node._
import com.github.caeus.elodin.lang.Path
import zio.{Ref, Task}

case class DeclParam(path: Path, index: Int)
case class DeclPath(node: Path, loc: Either[String, Int])
final class AnalyzeParams(accumulator: Ref[Map[Path, Set[DeclParam]]], root: CtxNode) {

  def extract(ctxNode: CtxNode, capturedParams: Set[DeclParam]): Task[Unit] = {
    accumulator
      .update { map =>
        map.updated(ctxNode.path, capturedParams)
      }
      .map(_ => ())
  }

  def walkDown(ctxNode: CtxNode, touchedPaths: Set[Path]): Task[Set[DeclParam]] = {
//    if (touchedPaths contains ctxNode.path) Task.succeed(Set.empty)
//    else {
//      val newTraversedPaths = touchedPaths + ctxNode.path
//      ctxNode.node match {
//        case moduleRefNode: RequireNode =>
//          ???
//        case _: LetNode =>
//          ctxNode.down.flatMap { body =>
//            walkDown(body, newTraversedPaths)
//          }
//        case lambdaNode: LambdaNode =>
//          val declParams =
//            (0 until lambdaNode.params.size).map(index => DeclParam(ctxNode.path, index)).toSet
//          for {
//            body <- ctxNode.down
//            deps <- walkDown(body, newTraversedPaths)
//            _    <- extract(ctxNode, deps)
//          } yield deps.diff(declParams)
//        case appNode: AppNode =>
//          for {
//            ctxs <- Task.sequence((0 until appNode.args.size).map(ctxNode.index))
//            depss <- Task.sequence(ctxs.map { body =>
//              walkDown(body, newTraversedPaths)
//            })
//          } yield depss.foldLeft(Set.empty[DeclParam])(_ ++ _)
//        case arrNode: ArrNode =>
//          for {
//            ctxs <- Task.sequence((0 until arrNode.items.size).map(ctxNode.index))
//            depss <- Task.sequence(ctxs.map { body =>
//              walkDown(body, newTraversedPaths)
//            })
//          } yield depss.foldLeft(Set.empty[DeclParam])(_ ++ _)
//        case dictNode: DictNode =>
//          for {
//            ctxs <- Task.sequence(dictNode.items.keySet.toSeq.map(ctxNode.key))
//            depss <- Task.sequence(ctxs.map { body =>
//              walkDown(body, newTraversedPaths)
//            })
//          } yield depss.foldLeft(Set.empty[DeclParam])(_ ++ _)
//        case refNode: RefNode =>
//          ctxNode.resolveRef(refNode.to) {
//            case IsNode(refdNode) =>
//              for {
//                deps <- walkDown(refdNode, newTraversedPaths)
//                _    <- extract(refdNode, deps)
//              } yield deps
//            case IsParam(ctx, index) =>
//              Task.succeed(Set(DeclParam(ctx.path, index)))
//            case IsExternal(module, id) =>
//              Task.succeed(Set.empty)
//          }
//        case _ => Task.succeed(Set.empty[DeclParam])
//      }
//    }
    ???
  }

  def run: Task[Map[Path, Set[DeclParam]]] = {
    for {
      deps   <- walkDown(root, Set.empty)
      _      <- extract(root, deps)
      result <- accumulator.get
    } yield result
  }
}
object AnalyzeParams {
  def apply(node: CtxNode): Task[Map[Path, Set[DeclParam]]] = {
    for {
      acum <- Ref.make(Map.empty[Path, Set[DeclParam]])
      compilation = new AnalyzeParams(acum, node)
      result <- compilation.run
    } yield result
  }
}
