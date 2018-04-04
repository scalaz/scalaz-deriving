// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz {

  import scala.AnyRef
  import scala.annotation.Annotation

  class deriving(val typeclasses: AnyRef*)  extends Annotation
  class xderiving(val typeclasses: AnyRef*) extends Annotation

  package macros {
    object DerivingMacros {
      def deriving[F[_], A]  = null
      def xderiving[F[_], A] = null
    }
  }

}
