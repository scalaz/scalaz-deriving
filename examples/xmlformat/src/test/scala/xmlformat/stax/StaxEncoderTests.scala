// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package stax

import org.scalatest._

class StaxEncoderTests extends FlatSpec with EncoderTestsParent {
  override def encode(t: XTag): String = StaxEncoder.encode(t)
}
