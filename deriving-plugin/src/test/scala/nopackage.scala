// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package testing

import testing.typeclasses.Cofoo
import scalaz.annotation.deriving

// annotation is not on a top level entry
object NotDerived {
  @deriving(Cofoo)
  object Inner
}
