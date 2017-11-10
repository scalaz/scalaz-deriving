// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

/** Implementation of Coapplicative in terms of a single, generic, method. */
trait CoapplicativeX[F[_]] extends Coapplicative[F] {
  import Catholics._

  def coapplyX[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z]

  override def map[A1, Z](a1: F[A1])(f: A1 => Z): F[Z] = coapply1(a1)(f)
  override def coapply1[Z, A1](a1: F[A1])(f: A1 => Z): F[Z] = {
    type L = A1 :: TNil
    coapplyX(Prod(Value(a1)))((c: Cop[L]) => f(to1(c)))
  }
  override def coapply2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(
    f: A1 \/ A2 => Z
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    coapplyX(LazyProd(a1, a2))((c: Cop[L]) => f(to2(c)))
  }
  override def coapply3[Z, A1, A2, A3](a1: => F[A1],
                                       a2: => F[A2],
                                       a3: => F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    coapplyX(LazyProd(a1, a2, a3))((c: Cop[L]) => f(to3(c)))
  }
  override def coapply4[Z, A1, A2, A3, A4](a1: => F[A1],
                                           a2: => F[A2],
                                           a3: => F[A3],
                                           a4: => F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    coapplyX(LazyProd(a1, a2, a3, a4))((c: Cop[L]) => f(to4(c)))
  }

}
object CoapplicativeX {
  @inline def apply[F[_]](implicit i: CoapplicativeX[F]): CoapplicativeX[F] = i
}
