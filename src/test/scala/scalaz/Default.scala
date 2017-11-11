// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Boolean, Int }

// a simple covariant typeclass
trait Default[A] {
  def default: A
}
object Default {
  @inline def apply[A](implicit i: Default[A]): Default[A] = i
  @inline def instance[A](a: => A): Default[A] = new Default[A] {
    override def default: A = a
  }

  implicit val int: Default[Int]         = instance(0)
  implicit val string: Default[String]   = instance("")
  implicit val boolean: Default[Boolean] = instance(false)

  implicit val derivation: CovariantTypeclassDerivation[Default] =
    new CovariantTypeclassDerivation[Default] {

      import iotaz._
      import iotaz.TList.Compute.{ Aux => ↦ }
      import iotaz.TList.Op.{ Map => ƒ }

      def applyX[A, Z, L <: TList, DefaultL <: TList](
        tcs: Prod[DefaultL]
      )(
        f: Prod[L] => Z
      )(
        implicit
        ev: λ[a => Name[Default[a]]] ƒ L ↦ DefaultL
      ): Default[Z] = instance {
        f(Prods.map(tcs)((d: Default[A]) => d.default))
      }

      def coapplyX[A, Z, L <: TList, TL <: TList](
        tcs: Prod[TL]
      )(
        f: Cop[L] => Z
      )(
        implicit
        ev: λ[a => Name[Default[a]]] ƒ L ↦ TL
      ): Default[Z] = instance {
        f(Cops.mapFirst(tcs)((d: Default[A]) => d.default))
      }

    }

}
