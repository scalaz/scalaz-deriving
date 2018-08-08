// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.annotation.Annotation

/** Generates boilerplate for implicit evidence on companion objects via
 * `scalaz.macros.DerivingMacro.deriving` */
final class deriving(val typeclasses: AnyRef*) extends Annotation

/** Generates boilerplate for implicit evidence on companion objects for single
 * valued data types via `scalaz.macros.DerivingMacro.xderiving` */
final class xderiving(val typeclasses: AnyRef*) extends Annotation
