// Copyright: 2017 https://gitlab.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
import play.api.libs.json._
import stalactite._

package wibble {
  @deriving(Format)
  sealed trait Ba@baz@z

  @deriving(Format)
  final case class F@foo@oo(string: String, int: Int) extends Baz

  @deriving(Format)
  final case class G@gaz@az[T](t: T) extends Baz
}

object Wobble {
  import wibble._

  implicitly[Format[Baz]]
  implicitly[Format[Foo]]
  implicitly[Format[Gaz[String]]]

}
