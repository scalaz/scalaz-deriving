// Copyright: 2017 - 2023 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package stax

import org.scalatest.flatspec.AnyFlatSpec

class StaxEncoderTests extends AnyFlatSpec with EncoderTestsParent {
  override def encode(t: XTag): String = StaxEncoder.encode(t)
}
