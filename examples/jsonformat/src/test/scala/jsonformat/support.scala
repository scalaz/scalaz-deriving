// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import JsDecoder.ops._
import JsEncoder.ops._

import scalaz._, Scalaz._

trait JsonTestSupport extends Matchers {

  def roundtrip[T: JsDecoder: JsEncoder](
    value: T,
    via: Option[String] = None
  ): Unit = {
    val json = value.toJson

    via match {
      case None =>
        println(
          s"check and add the following assertion: $value = ${PrettyPrinter(json)}"
        )
      case Some(expected) =>
        json.shouldBe(JsParser(expected))
    }

    val _ = json.as[T].shouldBe(value)
  }

  def roundtrip[T: JsDecoder: JsEncoder](value: T, via: String): Unit =
    roundtrip(value, Some(via))

  // inefficient constructors that are convenient in tests
  implicit class JsArrayCompanionOps(self: JsArray.type) {
    def apply(vs: JsValue*): JsArray = JsArray(vs.toList.toIList)
  }
  implicit class JsObjectCompanionOps(self: JsObject.type) {
    def apply(vs: (String, JsValue)*): JsObject = JsObject(vs.toList.toIList)
  }

}
