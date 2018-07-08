// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any, AnyVal, Int }
import scala.annotation.switch
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import ExtendedInvariantAlt._

/** Implement `Deriving` (N-arity) by wrapping `InvariantAlt` (fixed arity). */
final class ExtendedInvariantAlt[F[_]] private (
  private val F: InvariantAlt[F]
) extends ExtendedInvariantApplicative[F](F)
    with Deriving[F] {

  // I'm so sorry... I'm going to hell.

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
object ExtendedInvariantAlt {
  @inline def apply[F[_]](
    implicit F: ExtendedInvariantAlt[F]
  ): ExtendedInvariantAlt[F] = F

  /** Not implicit, to avoid breaking coherence. Always manually assign. */
  @inline def apply[F[_]](F: InvariantAlt[F]): ExtendedInvariantAlt[F] =
    new ExtendedInvariantAlt(F)

  private[scalaz] final implicit class UnsafeCops[T <: TList](
    private val self: Cop[T]
  ) extends AnyVal {
    // a completely unsafe operation that is useful when we have some runtime
    // information like we create a
    //
    //   Any :: Any :: Any :: TNil
    //
    // and we know it is an instance of the more general type A
    def as[A <: TList]: Cop[A] = self.asInstanceOf[Cop[A]]

    def shift(i: Int): Cop[T] =
      Cop.unsafeApply[T, Any](self.index + i, self.value)
  }
}
