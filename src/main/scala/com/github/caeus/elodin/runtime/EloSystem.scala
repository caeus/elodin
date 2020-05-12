package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.compiler.{EloScript, ModuleCompiler}
import com.github.caeus.elodin.runtime.Val.{Atomic, FnPointer}
import zio.{RefM, Task}

trait EloSystem {
  def register(script: EloScript): Task[Unit]

  def folderOf(pointer: PPointer): Task[EloFolder]

  def apply(cmd: String): Task[Val]

  final def eval(value: Val): Task[Atomic] = {
    value match {
      case atom: Atomic => Task.succeed(atom)
      case Val.Lazy(pointer, args) =>
        for {
          result0 <- fold(pointer, args.toList)
          result  <- eval(result0)
        } yield result
    }
  }

  final def fold(pointer: PPointer, from: List[Val]): Task[Atomic] = {
    for {
      folder <- folderOf(pointer)

      (taken, remaining) = from.splitAt(folder.arity)
      resultHead <- if (folder.arity > taken.length) {
                     Task.succeed(Val.Fn(pointer, taken))
                   } else folder.impl(taken).provide(this)
      continue <- (resultHead, remaining) match {
                   case (value, Nil) =>
                     Task.succeed(value)
                   case (fn: FnPointer, args) =>
                     fold(fn.pointer, fn.applyTo(args).args.toList)
                   case _ => Task.fail(new Exception("COREEEE ERROR"))
                 }
      result <- continue match {
                 case Val.Lazy(pointer, args) => fold(pointer, args.toList)
                 case atom: Atomic            => Task.succeed(atom)
               }
    } yield result
  }
}
object EloSystem{
  def make:Task[EloSystem]={
    RefM.make[Map[String, EloModule]](Map.empty).map{modules=>
      new DefaultEloSystem(modules)
    }
  }
}
final class DefaultEloSystem(modules: zio.RefM[Map[String, EloModule]]) extends EloSystem {
  override def register(script: EloScript): Task[Unit] = {
    modules.update {
      case modules if modules contains script.namespace =>
        Task.fail(new Exception("already defined"))
      case modules =>
        val compiler = new ModuleCompiler()
        compiler.compile(script).map { module =>
          modules.updated(script.namespace, module)
        }.provide(this)
    }
  }

  override def apply(cmd: String): Task[Val] = {
    val spliter = cmd.lastIndexOf(":")
    if (spliter >= 0) {
      val (module, member) = cmd.splitAt(spliter)
      modules.get
        .flatMap { modules =>
          Task.effect(modules(module))
        }
        .flatMap { module =>
          module(member)
        }
    } else {
      modules.get.flatMap { modules =>
        Task.effect(modules(cmd)).map(Val.Atom)
      }
    }
  }

  override def folderOf(pointer: PPointer): Task[EloFolder] =
    pointer match {
      case PPointer.Native(module, arity, member) =>
        modules.get
          .flatMap { modules =>
            Task.effect(modules(module))
          }
          .flatMap {
            case module: NativeModule => module.folder(member)
            case _                    => Task.fail(new Exception("Inconsistent state of pointer"))
          }
      case PPointer.Compiled(module, arity, member) =>
        modules.get
          .flatMap { modules =>
            Task.effect(modules(module))
          }
          .flatMap {
            case module: SrcModule => module.folder(member)
            case _                 => Task.fail(new Exception("Inconsistent state of pointer"))
          }
    }
}
