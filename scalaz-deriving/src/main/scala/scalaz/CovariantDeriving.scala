// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.inline

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._
import Prods._
import Cops._

/** A convenient API that typeclass authors may use to implement Altz */
abstract class CovariantDeriving[F[_]] extends Altz[F] {

  /**
   * Implementors provide the application of F[A] to a generic product A (F ~>
   * Id), receiving back the reconstructed concrete product Z.
   */
  def productz[Z](f: (F ~> Id) => Z): F[Z]

  /**
   * Implementers provide the optional application of F[A] to a generic
   * coproduct A (F ~> Maybe), receiving back a stream of reconstructed products
   * Z.
   */
  def coproductz[Z](f: (F ~> Maybe) => EphemeralStream[Z]): F[Z]

  final override def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = applyz(tcs)(f)

  final def applyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Prod[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    productz(((faa: (F ~> Id)) => f(Prods.map(tcs)(faa))))

  final override def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = altlyz(tcs)(f)

  final override def altlyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    coproductz((faa: (F ~> Maybe)) => Cops.mapMaybe(tcs)(faa).map(f))

  // derived combinators
  override def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B] =
    apply2(fa, f)((a, abc) => abc(a))

  override def point[Z](z: =>Z): F[Z] =
    applyz[Z, TNil, TNil](empty)(_ => z)

  override def map[A1, Z](a1: F[A1])(f: A1 => Z): F[Z] = {
    type L = A1 :: TNil
    applyz(Prod(Value(a1)))((a: Prod[L]) => f(to1T(a)))
  }

  override def apply2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    applyz(LazyProd(a1, a2))((as: Prod[L]) => f.tupled(to2T(as)))
  }
  override def apply3[A1, A2, A3, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    applyz(LazyProd(a1, a2, a3))((as: Prod[L]) => f.tupled(to3T(as)))
  }
  override def apply4[A1, A2, A3, A4, Z](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: (A1, A2, A3, A4) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    applyz(LazyProd(a1, a2, a3, a4))((as: Prod[L]) => f.tupled(to4T(as)))
  }
  // scalaz goes all the way to apply12, but we give up here for brevity

  override def alt[A](a1: =>F[A], a2: =>F[A]): F[A] = altly2(a1, a2) {
    case -\/(a) => a
    case \/-(a) => a
  }

  override def altly1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = {
    type L = A1 :: TNil
    altlyz(LazyProd(a1))((c: Cop[L]) => f(to1(c)))
  }
  override def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: A1 \/ A2 => Z
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    altlyz(LazyProd(a1, a2))((c: Cop[L]) => f(to2(c)))
  }
  override def altly3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    altlyz(LazyProd(a1, a2, a3))((c: Cop[L]) => f(to3(c)))
  }
  override def altly4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    altlyz(LazyProd(a1, a2, a3, a4))((c: Cop[L]) => f(to4(c)))
  }

}
object CovariantDeriving {
  @inline def apply[F[_]](
    implicit F: CovariantDeriving[F]
  ): CovariantDeriving[F] = F
}
