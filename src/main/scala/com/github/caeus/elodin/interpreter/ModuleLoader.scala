package com.github.caeus.elodin.interpreter

import zio.{RIO, Task}

case class NativeImpl(arity: Int, reducer: Seq[Val] => RIO[Interpreter, Val])

trait ModuleLoader {


  def get(name: String): Task[Val]

  def nativeImpl(name: String): Task[NativeImpl]
}

final class DefaultModuleLoader extends ModuleLoader {

  override def get(name: String): Task[Val] = ???

  override def nativeImpl(name: String): Task[NativeImpl] = ???
}
