// Copyright: 2017 - 2024 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz.annotation

import scala.annotation.Annotation

/**
 * Generates boilerplate for implicit evidence on companion objects via
 * `scalaz.macros.DerivingMacro.deriving`
 */
final class deriving(val typeclasses: AnyRef*) extends Annotation
