// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala._
import scala.reflect.macros.blackbox

import iotaz._

final class DerivezMacros(val c: blackbox.Context) {
  import c.universe._

  def gen[A, R <: TList, L <: TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R],
    evL: c.WeakTypeTag[L]
  ): Tree =
    Predef.???
}
