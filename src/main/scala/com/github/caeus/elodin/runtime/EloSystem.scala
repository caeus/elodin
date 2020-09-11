package com.github.caeus.elodin.runtime

import com.github.caeus.elodin.compiler.{DefaultModuleCompiler, ModuleCompiler}
import com.github.caeus.elodin.modules.{EloFolder, EloModule, NativeModule, SrcModule}
import com.github.caeus.elodin.nb.Script
import com.github.caeus.elodin.runtime.Val.{Atomic, FnPointer}
import com.typesafe.scalalogging.LazyLogging
import zio.{RefM, Task, UIO}

trait EloSystem extends LazyLogging {
  def register(script: Script): Task[Unit]

  def folderOf(pointer: PPointer): Task[EloFolder]

  def apply(cmd: String): Task[Val]

  final def atomize(value: Val): Task[Atomic] = {
    value match {
      case atom: Atomic => Task.succeed(atom)
      case Val.Lazy(pointer, args) =>
        for {
          result0 <- fold(pointer, args.toList)
          result  <- atomize(result0)
        } yield result
    }
  }

  final def fold(pointer: PPointer, from: List[Val]): Task[Atomic] = {
    for {
      folder            <- folderOf(pointer)
      (taken, remaining) = from.splitAt(folder.arity)
      resultHead <- if (folder.arity > taken.length) {

                     Task.succeed(Val.Fn(pointer, taken))
                   } else {
                     logger.info(s"Evaluaating $pointer... YEI!")
                     folder.impl(taken).provide(this)
                   }
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
object EloSystem {
  def make: UIO[EloSystem] = {
    RefM
      .make[Map[String, EloModule]](
        Map.empty
      )
      .map { modules =>
        new DefaultEloSystem(modules)
      }
  }
}
final class DefaultEloSystem(modules: zio.RefM[Map[String, EloModule]]) extends EloSystem {
  override def register(script: Script): Task[Unit] = {
    modules.update {
      case modules if modules contains script.namespace =>
        Task.fail(new Exception("already defined"))
      case modules =>
        val compiler = new DefaultModuleCompiler()
        compiler
          .compile(script)
          .map { module =>
            modules.updated(script.namespace, module)
          }
          .provide(this)
    }
  }

  override def apply(cmd: String): Task[Val] = {
    val spliter = cmd.lastIndexOf(":")
    if (spliter >= 0) {
      val (module, _member) = cmd.splitAt(spliter)
      val member            = _member.substring(1)
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
      case PPointer.Native(module, member) =>
        modules.get
          .flatMap { modules =>
            Task.effect(modules(module))
          }
          .flatMap {
            case module: NativeModule => module.folder(member)
            case _                    => Task.fail(new Exception("Inconsistent state of pointer"))
          }
      case PPointer.Compiled(module, member) =>
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
