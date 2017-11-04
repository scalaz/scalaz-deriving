// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

trait ProductiveX[F[_]] extends Productive[F] {
  // TODO
}
object ProductiveX {
  @inline def apply[F[_]](implicit i: ProductiveX[F]): ProductiveX[F] = i
}
