// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz.{ \/, ICons, INil }
import simulacrum._

@typeclass trait XDecoderTag[A] {
  def fromXTag(x: XTag): String \/ A

  final def asXDecoder: XDecoder[A] = {
    case XChildren(ICons(t, INil())) => fromXTag(t)
    case other                       => XDecoder.fail("one tag", other)
  }
}
