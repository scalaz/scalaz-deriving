// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package cord

import org.scalatest._

class TreeEncoderTests extends FlatSpec with EncoderTestsParent {
  override def encode(t: XTag): String = TreeEncoder.encode(t)
}
