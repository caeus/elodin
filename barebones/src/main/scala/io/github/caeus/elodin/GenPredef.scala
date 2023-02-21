package io.github.caeus.elodin

import zio.Task

import java.nio.file.{Path, Paths}

object GenPredef {

  private val compiler = compile.Compiler.make
  private val packager = compile.BundleWriter.make

  def release(in: String): Unit = {
    zio.Unsafe.unsafe(implicit unsafe =>
      zio.Runtime.default.unsafe.run(
        compileNPackage(
          folder = Paths.get("barebones/src/main/elodin").toAbsolutePath,
          target = Paths.get(in)
        )
      )
    )
  }

  private def compileNPackage(folder: Path, target: Path): Task[Unit] = {
    for {
      bundle <- compiler.compilePath(folder)
      _      <- packager.save(target, bundle)
    } yield ()
  }

}
