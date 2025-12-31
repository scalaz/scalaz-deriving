/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package testing

import scalaz.annotation.deriving
import testing.typeclasses.Cofoo

// annotation is not on a top level entry
object NotDerived {
  @deriving(Cofoo)
  object Inner
}
