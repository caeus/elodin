package com.github.caeus.elodin.interpreter

import com.github.caeus.elodin.interpreter.Val.Atom
import zio.{RIO, Task}

case class NativeImpl(arity: Int, reducer: Seq[Val] => RIO[Interpreter, Val])

trait FFI {

  def get(interpreter: Interpreter)(expr: Val): Task[Val]

  def nativeImpl(name: String): Task[NativeImpl]
}

final class DefaultFFI extends FFI {

  override def nativeImpl(name: String): Task[NativeImpl] = ???

  private def staticMethod(
      className: String,
      methodName: String,
      paramHints: Seq[String]
  ): Task[Val.Lazy] = {
    Task[Class[_]](Class.forName(className))
      .flatMap { clazz =>
        Task
          .collectAll(paramHints.map(name => Task[Class[_]](Class.forName(name))))
          .flatMap { params =>
            Task(clazz.getMethod(methodName, params: _*)).map(_ => ???)
          }
      }

  }

  override def get(interpreter: Interpreter)(expr: Val): Task[Val] = {
    Task.succeed(Atom(Void))
  }
}
