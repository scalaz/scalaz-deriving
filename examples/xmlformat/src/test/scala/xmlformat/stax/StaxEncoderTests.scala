/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package xmlformat
package stax

import org.scalatest.flatspec.AnyFlatSpec

class StaxEncoderTests extends AnyFlatSpec with EncoderTestsParent {
  override def encode(t: XTag): String = StaxEncoder.encode(t)
}
