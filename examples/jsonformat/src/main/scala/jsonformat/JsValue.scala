// Copyright: 2017 - 2025 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scalaz._, Scalaz._
import scalaz.annotation.deriving
import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazArbitrary.ilistArbitrary

@deriving(Show, Equal, Arbitrary)
sealed abstract class JsValue extends Product with Serializable {
  def widen: JsValue = this
}

final case object JsNull                                    extends JsValue
final case class JsObject(fields: IList[(String, JsValue)]) extends JsValue
final case class JsArray(elements: IList[JsValue])          extends JsValue
final case class JsBoolean(value: Boolean)                  extends JsValue
final case class JsString(value: String)                    extends JsValue
final case class JsDouble(value: Double)                    extends JsValue
final case class JsInteger(value: Long)                     extends JsValue
