// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.Predef.identity
import scala.{ inline, Any, AnyVal, Int }
import scala.annotation.switch
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import GenericDerives._

/**
 * Implement `Deriving` (N-arity) by wrapping `InvariantAlt` (fixed arity).
 *
 * This instance incurs a performance penalty and should only be used for legacy
 * code, prefer a direct implementation of `InvariantAlt`.
 */
final class GenericDerives[F[_]] private (
  private val F: InvariantAlt[F]
) extends Deriving[F] {

  // I'm so sorry... I'm going to hell.

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
  ): F[Z] = _xproductz(tcs.values.asInstanceOf[Seq[Name[F[Any]]]])(f, g)

  private def _xproductz[Z, L <: TList](
    tcs: Seq[Name[F[Any]]]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  ): F[Z] = (tcs.size: @switch) match {
    case 0 => F.xproduct0(f(Prod[TNil]().as[L]))
    case 1 =>
      type One = Any :: TNil
      val fz: Any => Z = a1 => f(Prod[One](a1).as[L])
      val gz: Z => Any = z => g(z).values(0)
      F.xproduct1(tcs(0).value)(fz, gz)
    case 2 =>
      type Two = Any :: Any :: TNil
      val fz: (Any, Any) => Z = (a1, a2) => f(Prod[Two](a1, a2).as[L])
      val gz: Z => (Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1))
      }
      F.xproduct2(tcs(0).value, tcs(1).value)(fz, gz)
    case 3 =>
      type Three = Any :: Any :: Any :: TNil
      val fz: (Any, Any, Any) => Z = (a1, a2, a3) =>
        f(Prod[Three](a1, a2, a3).as[L])
      val gz: Z => (Any, Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1), as(2))
      }
      F.xproduct3(tcs(0).value, tcs(1).value, tcs(2).value)(fz, gz)
    case 4 =>
      type Four = Any :: Any :: Any :: Any :: TNil
      val fz: (Any, Any, Any, Any) => Z = (a1, a2, a3, a4) =>
        f(Prod[Four](a1, a2, a3, a4).as[L])
      val gz: Z => (Any, Any, Any, Any) = z => {
        val as = g(z).values
        (as(0), as(1), as(2), as(3))
      }
      F.xproduct4(tcs(0).value, tcs(1).value, tcs(2).value, tcs(3).value)(
        fz,
        gz
      )
    case _ =>
      type Four = Any :: Any :: Any :: Any :: TNil

      val front: F[Prod[Four]] =
        _xproductz[Prod[Four], TList](tcs.take(4))(_.as[Four], _.as[TList])
      val back: F[Prod[TList]] =
        _xproductz[Prod[TList], TList](tcs.drop(4))(identity, identity)

      val fz: (Prod[Four], Prod[TList]) => Z =
        (a, b) => f(Prod.unsafeApply[L](a.values ++ b.values))
      val gz: Z => (Prod[Four], Prod[TList]) = { z =>
        val as = g(z).values
        (
          Prod.unsafeApply[Four](as.take(4)),
          Prod.unsafeApply[TList](as.drop(4))
        )
      }

      F.xproduct2(front, back)(fz, gz)
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
  ): F[Z] = _xcoproductz(tcs.values.asInstanceOf[Seq[Name[F[Any]]]])(f, g)

  private def _xcoproductz[Z, L <: TList](
    tcs: Seq[Name[F[Any]]]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  ): F[Z] = (tcs.size: @switch) match {
    case 1 =>
      val fz: Any => Z = a1 => f(Cops.from1(a1).as[L])
      val gz: Z => Any = z => g(z).value
      F.xcoproduct1(tcs(0).value)(fz, gz)
    case 2 =>
      type Two = Any :: Any :: TNil
      val fz: (Any \/ Any) => Z = e => f(Cops.from2(e).as[L])
      val gz: Z => (Any \/ Any) = z => Cops.to2(g(z).as[Two])
      F.xcoproduct2(tcs(0).value, tcs(1).value)(fz, gz)
    case 3 =>
      type Three = Any :: Any :: Any :: TNil
      val fz: (Any \/ (Any \/ Any)) => Z = e => f(Cops.from3(e).as[L])
      val gz: Z => (Any \/ (Any \/ Any)) = z => Cops.to3(g(z).as[Three])
      F.xcoproduct3(tcs(0).value, tcs(1).value, tcs(2).value)(fz, gz)

    case length =>
      type Four    = Any :: Any :: Any :: Any :: TNil
      type FourDis = Any \/ (Any \/ (Any \/ Any))
      val fe: FourDis => Cop[Four] = e => Cops.from4(e).shift(length - 4)
      val ge: Cop[Four] => FourDis = c => Cops.to4(c.shift(4 - length))
      val end: F[Cop[Four]] = F.xcoproduct4(
        tcs(length - 4).value,
        tcs(length - 3).value,
        tcs(length - 2).value,
        tcs(length - 1).value
      )(fe, ge)

      val front: F[Cop[TList]] =
        tcs
          .take(length - 4)
          .zipWithIndex
          .foldRight(
            end.asInstanceOf[F[Cop[TList]]]
          ) {
            case ((tc, i), acc) =>
              val ff: (Any \/ Cop[TList]) => Cop[TList] = {
                case -\/(a) => Cop.unsafeApply[TList, Any](i, a)
                case \/-(c) => c
              }
              val fg: Cop[TList] => (Any \/ Cop[TList]) = { c =>
                if (c.index == i) -\/(c.value)
                else \/-(c)
              }
              F.xcoproduct2(tc.value, acc)(ff, fg)
          }

      val fz: Cop[TList] => Z = c => f(c.as[L])
      val gz: Z => Cop[TList] = z => g(z).as[TList]
      F.xmap(front, fz, gz)
  }

}
object GenericDerives {
  @inline def apply[F[_]](implicit F: GenericDerives[F]): GenericDerives[F] = F

  /** Not implicit, to avoid breaking coherence. Always manually assign. */
  @inline def apply[F[_]](F: InvariantAlt[F]): GenericDerives[F] =
    new GenericDerives(F)

  private[scalaz] final implicit class UnsafeCops[T <: TList](
    private val self: Cop[T]
  ) extends AnyVal {
    // a completely unsafe operation that is useful when we have some runtime
    // information like we create a
    //
    //   Any :: Any :: Any :: TList
    //
    // and we know it is an instance of the more general type TList[A]
    def as[A <: TList]: Cop[A] = self.asInstanceOf[Cop[A]]

    def shift(i: Int): Cop[T] =
      Cop.unsafeApply[T, Any](self.index + i, self.value)
  }
  private[scalaz] final implicit class UnsafeProds[T <: TList](
    private val self: Prod[T]
  ) extends AnyVal {
    def as[A <: TList]: Prod[A] = self.asInstanceOf[Prod[A]]
  }
}
