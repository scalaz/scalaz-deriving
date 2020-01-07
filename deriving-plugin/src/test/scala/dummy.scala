// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

import scala.Null

package scalaz {

  import scala.AnyRef
  import scala.annotation.Annotation

  package annotation {
    class deriving(val typeclasses: AnyRef*)  extends Annotation
    class xderiving(val typeclasses: AnyRef*) extends Annotation
  }

  package macros {
    object DerivingMacros {
      def deriving[F[_], A]: Null  = null // scalafix:ok
      def xderiving[F[_], A]: Null = null // scalafix:ok
    }
  }

}
