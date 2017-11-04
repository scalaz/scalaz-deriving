// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

trait CoproductiveX[F[_]] extends Coproductive[F] {
  // TODO
}
object CoproductiveX {
  @inline def apply[F[_]](implicit i: CoproductiveX[F]): CoproductiveX[F] = i
}
