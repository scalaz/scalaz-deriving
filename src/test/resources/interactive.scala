// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
import play.api.libs.json
import play.api.libs.json._
import stalactite._
import shapeless.the

package wibble {
  @deriving(json.Format)
  sealed trait Ba@baz@z

  @deriving(json.Format)
  final case class F@foo@oo(string: String, int: Int) extends Baz

  @deriving(json.Format)
  final case class G@gaz@az[T](t: T) extends Baz
}

object Wobble {
  import wibble._

  //the[json.Format[Baz]]
  implicitly[json.Format[Foo]]
  //the[json.Format[Gaz[String]]]

}
