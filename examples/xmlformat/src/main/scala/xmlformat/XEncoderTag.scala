// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import simulacrum._

/** See docs on DerivedXEncoder */
@typeclass trait XEncoderTag[A] {
  def toXTag(a: A): XTag
}
