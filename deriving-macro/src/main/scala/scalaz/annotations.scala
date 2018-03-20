// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.annotation.StaticAnnotation

/** Generates boilerplate for implicit evidence on companion objects via
 * `scalaz.macros.DerivingMacro.deriving` */
class deriving(val typeclasses: AnyRef*) extends StaticAnnotation

/** Generates boilerplate for implicit evidence on companion objects for single
 * valued data types via `scalaz.macros.DerivingMacro.xderiving` */
class xderiving(val typeclasses: AnyRef*) extends StaticAnnotation

/** use to flag an explicit or implicit parameter as known to be unused
 * https://github.com/scala/bug/issues/10790
 * https://github.com/scalaz/scalaz/issues/1669
 */
class unused extends deprecated("unused", "")
