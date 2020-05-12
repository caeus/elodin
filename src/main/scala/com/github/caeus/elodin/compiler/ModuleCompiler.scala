package com.github.caeus.elodin.compiler

import com.github.caeus.elodin.compiler.Dig.Path
import com.github.caeus.elodin.compiler.EloScript.StrScript
import com.github.caeus.elodin.compiler.Lexcope.{
  Root,
  WhenApply,
  WhenBool,
  WhenFloat,
  WhenFn,
  WhenInt,
  WhenLet,
  WhenRef,
  WhenReq,
  WhenText
}
import com.github.caeus.elodin.frontend.{Lexer, Parser}
import com.github.caeus.elodin.lang.Node
import com.github.caeus.elodin.lang.Node._
import com.github.caeus.elodin.runtime.Val.FnPointer
import com.github.caeus.elodin.runtime._
import zio.{RIO, Ref, Task}

case class Shifter(arity: Int, shift: Shift) {}

case class ModuleInit(name: String, shifters: IndexedSeq[Shifter])

sealed trait Dig {
  override def toString: String =
    this match {
      case Dig.Down         => "_"
      case Dig.Key(value)   => value
      case Dig.Index(value) => s"[$value]"
    }
}
object Dig {
  type Path = Seq[Dig]
  case object Down              extends Dig
  case class Key(value: String) extends Dig
  case class Index(value: Int)  extends Dig

}

sealed trait Lexcope[N <: Node] {
  def node: N
  def path: Dig.Path
  def maybeParent: Option[Lexcope[Node]]
  final def resolve(ref: String): Option[Either[Lexcope[Node], DeclParam]] = {
    this.widen match {
      case WhenFn(lexcope) if lexcope.node.params.contains(ref) =>
        Some(Right(DeclParam(path, lexcope.node.params.lastIndexOf(ref))))
      case WhenLet(lexcope) if lexcope.node.bindings.contains(ref) =>
        Some(Left(lexcope.binding(ref).get))
      case _ =>
        maybeParent.flatMap(_.resolve(ref))
    }
  }

  override def toString: String = path.mkString("\\", ".", "")
}

object Lexcope {
  case class Root[N <: Node](node: N) extends Lexcope[N] {
    override def path: Path                              = Vector.empty
    override lazy val maybeParent: Option[Lexcope[Node]] = None
  }
  case class Child[N <: Node](node: N, dig: Dig, parent: Lexcope[Node]) extends Lexcope[N] {
    override lazy val path: Path = parent.path.appended(dig)

    override lazy val maybeParent: Option[Lexcope[Node]] = Some(parent)
  }
  sealed class When[N <: Node: Manifest] {
    def unapply(lexcope: Lexcope[Node]): Option[Lexcope[N]] = {
      lexcope.node match {
        case _: N => Some(lexcope.asInstanceOf[Lexcope[N]])
        case _    => None
      }
    }
  }
  object WhenLet   extends When[LetNode]
  object WhenFn    extends When[FnNode]
  object WhenRef   extends When[RefNode]
  object WhenReq   extends When[ReqNode]
  object WhenApply extends When[ApplyNode]
  object WhenText  extends When[TextNode]
  object WhenFloat extends When[FloatNode]
  object WhenInt   extends When[IntNode]
  object WhenBool  extends When[BoolNode]

  implicit final class LexcopeOps[N <: Node](private val value: Lexcope[N]) extends AnyVal {
    def widen: Lexcope[Node] = value.asInstanceOf[Lexcope[Node]]
  }
  implicit final class LetLexcopeOps(private val value: Lexcope[LetNode]) extends AnyVal {
    def body: Lexcope[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def binding(to: String): Option[Lexcope[Node]] = {
      value.node.bindings.get(to).map { node =>
        Child(node, Dig.Key(to), value.widen)
      }
    }
  }
  implicit final class FnLexcopeOps(private val value: Lexcope[FnNode]) extends AnyVal {
    def body: Lexcope[Node] = {
      Child(value.node.body, Dig.Down, value.widen)
    }
    def params: Set[DeclParam] = {
      value.node.params.zipWithIndex.map {
        case (_, index) => DeclParam(value.path, index)
      }.toSet
    }
  }
  implicit final class ApplyLexcopeOps(private val value: Lexcope[ApplyNode]) extends AnyVal {
    def args: Seq[Lexcope[Node]] = {
      value.node.args.zipWithIndex.map {
        case (node, index) => Child(node, Dig.Index(index), value.widen)
      }
    }
  }

}
case class DeclParam(path: Dig.Path, index: Int)
sealed trait Shift {
  private final def applyRec(values: Seq[Val])(shift: Shift): RIO[EloSystem, Val] = {
    shift match {
      case Shift.Atom(to)   => RIO.succeed(to)
      case Shift.Arg(index) => RIO.effect(values(index))
      case Shift.Apply(shifts: Seq[Shift]) =>
        RIO
          .collectAll(shifts.map(applyRec(values)))
          .flatMap {
            case (fn: FnPointer) :: args =>
              RIO.succeed(fn.applyTo(args))
            case Nil => RIO.succeed(Val.Atom(()))
            case _   => RIO.fail(new Exception("NOT APLICABLE"))
          }
      case Shift.System(value) =>
        RIO.environment[EloSystem].flatMap(_.apply(value))
    }
  }
  final def apply(values: Seq[Val]): RIO[EloSystem, Val] = {
    applyRec(values)(this)
  }
}
object Shift {
  case class Atom(value: Val)        extends Shift
  case class Apply(args: Seq[Shift]) extends Shift
  case class Arg(index: Int)         extends Shift
  case class System(value: String)   extends Shift
}

class ModuleCompiler {
  def compile(script: EloScript): RIO[EloSystem, EloModule] = {
    script match {
      case StrScript(_, code) =>
        for {
          tokens <- new Lexer().lex(code)
          node   <- new Parser().parse(tokens)
          r      <- compile(script.namespace, node)
        } yield r
    }

  }

  //Ref[Map[Path, Set[DeclParam]]]

  def walkDown(
      lexcope: Lexcope[Node],
      touchedPaths: Set[Dig.Path],
      emitted: Ref[Map[Lexcope[Node], Set[DeclParam]]]
  ): Task[Set[DeclParam]] = {
    def emit(lexcope: Lexcope[Node])(capturedParams: Set[DeclParam]) = {
      emitted
        .update { map =>
          map.updated(lexcope, capturedParams)
        }
        .map(_ => capturedParams)
    }
    if (touchedPaths.contains(lexcope.path)) {
      Task.succeed(Set.empty)
    } else {
      val newTouchedPaths = touchedPaths + lexcope.path
      lexcope.widen match {
        case WhenLet(lexcope) =>
          walkDown(lexcope.body, newTouchedPaths, emitted)
        case WhenFn(lexcope) =>
          walkDown(lexcope.body, newTouchedPaths, emitted)
            .map(_ diff lexcope.params)
            .flatMap(emit(lexcope.widen))
        case WhenApply(xcope) =>
          Task
            .collectAll(xcope.args.map(s => walkDown(s, newTouchedPaths, emitted)))
            .map(_.reduce(_ ++ _))
        case WhenRef(lexcope) =>
          lexcope
            .resolve(lexcope.node.to)
            .map {
              case Left(xcope) =>
                walkDown(xcope, touchedPaths, emitted)
                  .flatMap(emit(xcope))
              case Right(param) => Task.succeed(Set(param))
            }
            .getOrElse(Task.fail(new Exception("WhASODASID")))
        case _ => Task.succeed(Set.empty)
      }
    }
  }

  def choosePaths(node: Node): Task[Map[Lexcope[Node], Set[DeclParam]]] = {
    for {
      emit  <- Ref.make[Map[Lexcope[Node], Set[DeclParam]]](Map.empty)
      root   = Root(node)
      empty <- walkDown(root, Set.empty, emit)
      _ <- if (empty.isEmpty) { Task.succeed(()) }
          else Task.fail(new Exception("Never happens, i guess"))
      paths <- emit.get
    } yield paths.updated(root, Set.empty)
  }

  def createInit(namespace: String, chosenNodes: Map[Lexcope[Node], Seq[DeclParam]]): ModuleInit = {

    val withIndex: Seq[(Lexcope[Node], Int)] = chosenNodes.toSeq
      .sortBy {
        case (lexcope, params) =>
          lexcope.path.length
      }
      .map(_._1)
      .zipWithIndex

    val indexes: Map[Lexcope[Node], Int] = withIndex.toMap

    def headerOf(node: Lexcope[Node]) = {
      chosenNodes(node).toSeq
        .appendedAll(node match {
          case WhenFn(scope) => scope.params.toSeq
          case _             => Nil
        })
        .sortBy(p => p.path.length -> p.index)
    }
    val pointers: Map[Lexcope[Node], Val.Lazy] = indexes.toSeq.map {
      case (node, index) =>
        node ->
          Val.Lazy(
            PPointer.Compiled(namespace, headerOf(node).size, indexes(node)),
            Nil
          )
    }.toMap
    def invocationShift(node: Lexcope[Node], enclosing: Seq[DeclParam]): Shift = {
      chosenNodes
        .get(node)
        .map { usedParams =>
          //FIXME should be an index with a specific class
          Shift.Apply(Seq(Shift.Atom(pointers(node))).appendedAll(usedParams.map { usedParam =>
            val i = enclosing.indexOf(usedParam)
            i.ensuring(
              i >= 0, {
                println(enclosing)
                println(usedParam)
                "Assertion failed GONORREA"
              }
            )
            Shift.Arg(i)
          }))
        }
        .getOrElse(toShift(node, enclosing))
    }

    def toShift(node: Lexcope[Node], header: Seq[DeclParam]): Shift = {

      node match {
        case WhenFn(xcope) =>
          invocationShift(xcope.body, header)
        case WhenApply(lexcope) =>
          Shift.Apply(lexcope.args.map(x => invocationShift(x, header)))
        case WhenLet(lexcope) =>
          invocationShift(lexcope.body, header)
        case WhenRef(lexcope) =>
          lexcope.resolve(lexcope.node.to).get match {
            case Left(refdLexcope) =>
              invocationShift(refdLexcope, header)
            case Right(param) =>
              val i = header.indexOf(param)
              i.ensuring(_ >= 0)
              Shift.Arg(i)
          }
        case WhenReq(lexcope)   => Shift.System(lexcope.node.to)
        case WhenText(lexcope)  => Shift.Atom(Val.Atom(lexcope.node.value))
        case WhenFloat(lexcope) => Shift.Atom(Val.Atom(lexcope.node.value))
        case WhenInt(lexcope)   => Shift.Atom(Val.Atom(lexcope.node.value))
        case WhenBool(lexcope)  => Shift.Atom(Val.Atom(lexcope.node.value))
      }
    }

    def toCombinator(node: Lexcope[Node]): Shifter = {
      val header = headerOf(node)
      Shifter(arity = header.length, toShift(node, header))
    }
    ModuleInit(namespace, withIndex.map(_._1).map(toCombinator).toIndexedSeq)
  }

  def compile(namespace: String, node: Node): RIO[EloSystem, EloModule] = {
    for {
      paths <-
        choosePaths(node).map(_.view.mapValues(_.toSeq.sortBy(p => p.path.length -> p.index)).toMap)
      _       = println(paths.mkString("\n"))
      init   <- Task.effect(createInit(namespace, paths))
      module <- SrcModule.build(namespace, init)
    } yield module
  }
}
