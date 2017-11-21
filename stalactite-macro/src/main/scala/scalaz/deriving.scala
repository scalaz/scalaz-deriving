// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.{ Any, AnyRef }

@compileTimeOnly("deriving annotation should have been removed")
class deriving(val typeclasses: AnyRef*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro stalactite.DerivingMacros.generateImplicits
}
