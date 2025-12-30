/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package testing

import testing.typeclasses.Cofoo
import scalaz.annotation.deriving

// annotation is not on a top level entry
object NotDerived {
  @deriving(Cofoo)
  object Inner
}
