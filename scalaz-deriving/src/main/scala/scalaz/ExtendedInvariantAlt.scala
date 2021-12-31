// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any }
import scala.annotation.switch
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.::
import Cops.ops._

/** Implement `Deriving` (N-arity) by wrapping `InvariantAlt` (fixed arity). */
final class ExtendedInvariantAlt[F[_]] private (
  private[this] val F: InvariantAlt[F]
) extends ExtendedInvariantApplicative[F](F)
    with Deriving[F] {

  // I'm so sorry... I'm going to hell.

  override def xcoproductz[Z, A <: TList, FA <: TList](
    tcs: Prod[FA]
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  )(implicit
    ev: A PairedWith FA
  ): F[Z] = _xcoproductz(tcs.values.asInstanceOf[Seq[Name[F[Any]]]])(f, g)

  private def _xcoproductz[Z, A <: TList](
    tcs: Seq[Name[F[Any]]]
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  ): F[Z] =
    (tcs.size: @switch) match {
      case 1 =>
        val fz: Any => Z = a1 => f(Cops.from1(a1).as[A])
        val gz: Z => Any = z => g(z).value
        F.xcoproduct1(tcs(0).value)(fz, gz)
      case 2 =>
        type Two = Any :: Any :: TNil
        val fz: (Any \/ Any) => Z = e => f(Cops.from2(e).as[A])
        val gz: Z => (Any \/ Any) = z => Cops.to2(g(z).as[Two])
        F.xcoproduct2(tcs(0).value, tcs(1).value)(fz, gz)
      case 3 =>
        type Three = Any :: Any :: Any :: TNil
        val fz: (Any \/ (Any \/ Any)) => Z = e => f(Cops.from3(e).as[A])
        val gz: Z => (Any \/ (Any \/ Any)) = z => Cops.to3(g(z).as[Three])
        F.xcoproduct3(tcs(0).value, tcs(1).value, tcs(2).value)(fz, gz)

      case length =>
        type Four    = Any :: Any :: Any :: Any :: TNil
        type FourDis = Any \/ (Any \/ (Any \/ Any))
        val fe: FourDis => Cop[Four] = e => Cops.from4(e).shift(length - 4)
        val ge: Cop[Four] => FourDis = c => Cops.to4(c.shift(4 - length))
        val end: F[Cop[Four]]        = F.xcoproduct4(
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
            ) { case ((tc, i), acc) =>
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

        val fz: Cop[TList] => Z = c => f(c.as[A])
        val gz: Z => Cop[TList] = z => g(z).as[TList]
        F.xmap(front, fz, gz)
    }

}
object ExtendedInvariantAlt {
  @inline def apply[F[_]](implicit
    F: ExtendedInvariantAlt[F]
  ): ExtendedInvariantAlt[F] = F

  /** Not implicit, to avoid breaking coherence. Always manually assign. */
  @inline def apply[F[_]](F: InvariantAlt[F]): ExtendedInvariantAlt[F] =
    new ExtendedInvariantAlt(F)

}
