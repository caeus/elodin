package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Ast9.GuestModule
import io.github.caeus.elodin.compile.util.ListRec
import zio.{Has, Task, ZIO}

import java.nio.file.Path
import scala.io.Source

object Compiler {
  type Box = Has[Service]
  trait Service {
    def compile(name: String, code: String): Task[Bundle]

    private def compileSingle(root: Path, path: Path): Task[Bundle] = {
      for {

        code <- ZIO(Source.fromFile(path.toFile))
                 .toManaged(c => ZIO(c.close()).orDie)
                 .map(_.mkString)
                 .useNow
        module <- compile(root.relativize(path).toString, code)
      } yield module
    }
    final def compilePath(path: Path): Task[Bundle] = {
      val root = path.toAbsolutePath.normalize()
      for {
        modulePaths <- ListRec(root)(_.getFileName.toString.endsWith(".elodin"))
        bundles <- ZIO
                    .collectAll(modulePaths.map(p => compileSingle(root, p)))
        bundle <- ZIO.fromEither(Bundle.merge(bundles: _*)).mapError { err =>
                   new IllegalArgumentException(s"Conflicts in path $path: $err")
                 }
      } yield bundle
    }
  }
  final class LiveService extends Service {
    private val lexer      = Lexer.make
    private val ast0Parser = Ast0Parser.make
    private val ast5Parser = Ast5Parser.make
    private val ast9Parser = Ast9Parser.make

    override def compile(name: String, code: String): Task[Bundle] = {
      for {
        tokens <- lexer.lex(code)
        ast0   <- ast0Parser.parse(tokens)
        ast5   <- ast5Parser.parse(ast0)
        ast9   <- ast9Parser.parse(name, ast5)
      } yield Bundle.single(name, ast9)
    }
  }
  def make: Service = new LiveService
}
