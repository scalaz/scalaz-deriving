/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.benchmarks

import scalaz.{ @@, Tags }
import scalaz.annotation.deriving
import jsonformat._

package s {
  final case class TradeTemplate(
    otc: Option[Boolean] @@ Tags.Last
  )
  object TradeTemplate {
    implicit val encoder: JsEncoder[TradeTemplate] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[TradeTemplate] = DerivedProductJsDecoder.gen
  }
}

package m {
  @deriving(JsEncoder, JsDecoder)
  final case class TradeTemplate(
    otc: Option[Boolean] @@ Tags.Last
  )
}
