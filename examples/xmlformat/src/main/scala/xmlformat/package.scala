// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._

sealed trait XAttribute

object `package` {
  val XAttribute = Tag.of[XAttribute]
}
