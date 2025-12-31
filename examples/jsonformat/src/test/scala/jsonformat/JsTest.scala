/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat

import JsDecoder.ops.*
import JsEncoder.ops.*
import internal.FastToIList.*
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scalaz.*
import scalaz.Scalaz.*

abstract class JsTest
    extends AnyFlatSpec
    with NonImplicitAssertions
    with ScalaCheckDrivenPropertyChecks {
  type Position = org.scalactic.source.Position
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
    def apply(vs: JsValue*): JsArray =
      JsArray(vs.toIList)
  }
  implicit class JsObjectCompanionOps(self: JsObject.type) {
    def apply(vs: (String, JsValue)*): JsObject =
      JsObject(vs.toIList)
  }

}
