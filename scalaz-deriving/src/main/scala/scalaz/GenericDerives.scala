// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any }
import scala.annotation.switch
import scala.collection.immutable.{ List, Nil }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

/**
 * Implement `Deriving` (N-arity) by wrapping `Derives` (fixed arity).
 *
 * This instance incurs a performance penalty and should only be used for legacy
 * code, prefer a direct implementation of `Derives`.
 */
final class GenericDerives[F[_]] private (
  private val F: Derives[F]
) extends Deriving[F] {

  override def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = (tcs.values.size: @switch) match {
    case 0 => F.xproduct0(f(Prod.unsafeApply[L](Nil)))
    case 1 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fz: Any => Z      = a1 => f(Prod.unsafeApply[L](List(a1)))
      val gz: Z => Any      = z => g(z).values(0)
      F.xproduct1(fa1.value)(fz, gz)
    case 2 =>
      val fa1: Name[F[Any]]   = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]]   = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fz: (Any, Any) => Z = (a1, a2) => f(Prod.unsafeApply[L](List(a1, a2)))
      val gz: Z => (Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1))
      }
      F.xproduct2(fa1.value, fa2.value)(fz, gz)
    case 3 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]] = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fa3: Name[F[Any]] = tcs.values(2).asInstanceOf[Name[F[Any]]]
      val fz: (Any, Any, Any) => Z = (a1, a2, a3) =>
        f(Prod.unsafeApply[L](List(a1, a2, a3)))
      val gz: Z => (Any, Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1), as(2))
      }
      F.xproduct3(fa1.value, fa2.value, fa3.value)(fz, gz)
    case 4 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]] = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fa3: Name[F[Any]] = tcs.values(2).asInstanceOf[Name[F[Any]]]
      val fa4: Name[F[Any]] = tcs.values(3).asInstanceOf[Name[F[Any]]]
      val fz: (Any, Any, Any, Any) => Z = (a1, a2, a3, a4) =>
        f(Prod.unsafeApply[L](List(a1, a2, a3, a4)))
      val gz: Z => (Any, Any, Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1), as(2), as(3))
      }
      F.xproduct4(fa1.value, fa2.value, fa3.value, fa4.value)(fz, gz)
    case _ =>
      scala.sys.error("https://gitlab.com/fommil/scalaz-deriving/issues/88")
  }

  override def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = (tcs.values.size: @switch) match {
    case 1 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fz: Any => Z      = a1 => f(Cop.unsafeApply[L, Any](0, a1))
      val gz: Z => Any      = z => g(z).value
      F.xcoproduct1(fa1.value)(fz, gz)
    case 2 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]] = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fz: (Any \/ Any) => Z = {
        case -\/(a1) => f(Cop.unsafeApply[L, Any](0, a1))
        case \/-(a2) => f(Cop.unsafeApply[L, Any](1, a2))
      }
      val gz: Z => (Any \/ Any) = z => {
        val as = g(z)
        (as.index: @switch) match {
          case 0 => -\/(as.value)
          case 1 => \/-(as.value)
        }
      }
      F.xcoproduct2(fa1.value, fa2.value)(fz, gz)
    case 3 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]] = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fa3: Name[F[Any]] = tcs.values(2).asInstanceOf[Name[F[Any]]]
      val fz: (Any \/ (Any \/ Any)) => Z = {
        case -\/(a1)      => f(Cop.unsafeApply[L, Any](0, a1))
        case \/-(-\/(a2)) => f(Cop.unsafeApply[L, Any](1, a2))
        case \/-(\/-(a3)) => f(Cop.unsafeApply[L, Any](2, a3))
      }
      val gz: Z => (Any \/ (Any \/ Any)) = z => {
        val as = g(z)
        (as.index: @switch) match {
          case 0 => -\/(as.value)
          case 1 => \/-(-\/(as.value))
          case 2 => \/-(\/-(as.value))
        }
      }
      F.xcoproduct3(fa1.value, fa2.value, fa3.value)(fz, gz)
    case 4 =>
      val fa1: Name[F[Any]] = tcs.values(0).asInstanceOf[Name[F[Any]]]
      val fa2: Name[F[Any]] = tcs.values(1).asInstanceOf[Name[F[Any]]]
      val fa3: Name[F[Any]] = tcs.values(2).asInstanceOf[Name[F[Any]]]
      val fa4: Name[F[Any]] = tcs.values(3).asInstanceOf[Name[F[Any]]]
      val fz: (Any \/ (Any \/ (Any \/ Any))) => Z = {
        case -\/(a1)           => f(Cop.unsafeApply[L, Any](0, a1))
        case \/-(-\/(a2))      => f(Cop.unsafeApply[L, Any](1, a2))
        case \/-(\/-(-\/(a3))) => f(Cop.unsafeApply[L, Any](2, a3))
        case \/-(\/-(\/-(a4))) => f(Cop.unsafeApply[L, Any](3, a4))
      }
      val gz: Z => (Any \/ (Any \/ (Any \/ Any))) = z => {
        val as = g(z)
        (as.index: @switch) match {
          case 0 => -\/(as.value)
          case 1 => \/-(-\/(as.value))
          case 2 => \/-(\/-(-\/(as.value)))
          case 3 => \/-(\/-(\/-(as.value)))
        }
      }
      F.xcoproduct4(fa1.value, fa2.value, fa3.value, fa4.value)(fz, gz)
    case _ =>
      scala.sys.error("https://gitlab.com/fommil/scalaz-deriving/issues/88")
  }

}
object GenericDerives {
  @inline def apply[F[_]](implicit F: GenericDerives[F]): GenericDerives[F] = F

  /** Not implicit, to avoid breaking coherence. Always manually assign. */
  @inline def apply[F[_]](F: Derives[F]): GenericDerives[F] =
    new GenericDerives(F)

}
