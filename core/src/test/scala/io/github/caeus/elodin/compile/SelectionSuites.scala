package io.github.caeus.elodin.compile

import io.github.caeus.elodin.compile.Node.ImportNode
import io.github.caeus.elodin.core.{Archive, Book, Thunk, ThunkRef}
import zio.test.Assertion._
import zio.test._

object SelectionSuites extends DefaultRunnableSpec {
  val archive: Archive = new Archive {
    val members = "abcdefghijklmnopqrstuwxyz".toSeq.map(_.toString).toSet

    override def thunkAt(ref: ThunkRef): Option[Thunk] = ???

    override def enrichedWith(books: Seq[Book]): Archive = ???

    override def thunkRefTo(module: String, name: String): Option[ThunkRef] = {
      println(module->name)
      if (module == "testmodule") {
        if (members contains name) {
          Some(ThunkRef(foreign = false,module, name))
        } else None
      } else None
    }
  }

  def ofSelection(selection: Node.Selection, prefix: Option[String] = None): Ctx = {
    Ctx.fromNode(
      ImportNode(
        "testmodule",
        selection,
        prefix,
        Node.RefNode("_")
      )
    )
  }

  override def spec: ZSpec[environment.TestEnvironment, Any] = {
    suite("Import.Selection")(test("works") {
      assert(
        ofSelection(
          Node.Selection.Only(
            values = Map("newa" -> "a"): Map[String, String]
          )
        )
      ) {
        hasField[Ctx, Option[ResolvedRef]]("newa", _.resolveRef(archive)("newa"), isSome) &&
        //hasField[Ctx, Option[ResolvedRef]]("a", _.resolveRef(archive)("a"), isNone) &&
        hasField[Ctx, Option[ResolvedRef]]("d", _.resolveRef(archive)("d"), isNone) &&
        hasField[Ctx, Option[ResolvedRef]]("b", _.resolveRef(archive)("b"), isNone)
      }

    })
  }
}
