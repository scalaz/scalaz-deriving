// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import JsDecoder.ops._
import JsEncoder.ops._

import scalaz._, Scalaz._

abstract class JsTest
    extends FlatSpec
    with NonImplicitAssertions
    with GeneratorDrivenPropertyChecks {
  type Position  = org.scalactic.source.Position
  type Assertion = org.scalatest.Assertion

  implicit class EncoderHelper[T: JsEncoder](t: T) {
    def jsonString: String = CompactPrinter(t.toJson)
  }

  implicit class DecoderHelper(s: String) {
    def parseAs[A: JsDecoder]: String \/ A =
      JsParser(s) >>= (_.as[A])
  }

  // inefficient constructors that are convenient in tests
  implicit class JsArrayCompanionOps(self: JsArray.type) {
    def apply(vs: JsValue*): JsArray = JsArray(vs.toList.toIList)
  }
  implicit class JsObjectCompanionOps(self: JsObject.type) {
    def apply(vs: (String, JsValue)*): JsObject = JsObject(vs.toList.toIList)
  }

}
