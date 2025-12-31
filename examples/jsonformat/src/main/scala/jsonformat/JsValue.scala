/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat

import org.scalacheck.Arbitrary
import scalaz.*
import scalaz.Scalaz.*
import scalaz.annotation.deriving
import scalaz.scalacheck.ScalazArbitrary.ilistArbitrary

@deriving(Show, Equal, Arbitrary)
sealed abstract class JsValue extends Product with Serializable {
  def widen: JsValue = this
}

final case object JsNull extends JsValue
final case class JsObject(fields: IList[(String, JsValue)]) extends JsValue
final case class JsArray(elements: IList[JsValue]) extends JsValue
final case class JsBoolean(value: Boolean) extends JsValue
final case class JsString(value: String) extends JsValue
final case class JsDouble(value: Double) extends JsValue
final case class JsInteger(value: Long) extends JsValue
