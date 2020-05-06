package com.github.caeus.elodin.interpreter.runtime

import com.github.caeus.elodin.interpreter.Val
import zio.Task

trait HasProps {
  def get(key: String): Task[Val]
}
final class Dict(props: Map[String, Val]) {
  def get(key: String) =
    props
      .get(key)
      .map(Task.succeed)
      .getOrElse(Task.fail(new Exception(s"Doesn't have property $key")))
}
object Dict {
  def apply(props: (String, Val)*): Dict = {
    new Dict(props.toMap)
  }
}
object Test {
 ::
  sun.misc.Unsafe.s
  Dict(
    "+" -> ???,
    "-" -> ???
  )

}
