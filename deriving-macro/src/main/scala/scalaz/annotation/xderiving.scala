// Copyright: 2017 - 2023 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz.annotation

import scala.annotation.Annotation

/**
 * Generates boilerplate for implicit evidence on companion objects for single
 * valued data types via `scalaz.macros.DerivingMacro.xderiving`
 */
final class xderiving(val typeclasses: AnyRef*) extends Annotation
