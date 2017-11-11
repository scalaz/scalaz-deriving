// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

// TODO: support arbitrary arity an the invariant level. i.e.
// CoapplicativeCodivideX and ApplicativeDivisibleX.
//
// allows typeclass derivation for products, coproducts and AnyVal
trait Derived[F[_]]
    extends CoapplicativeCodivide[F]
    with ApplicativeDivisible[F]
object Derived {
  @inline def apply[F[_]](
    implicit i: Derived[F]
  ): Derived[F] = i

  import Scalaz._
  import Maybe.Just

  // should really be on the companion of Equal
  implicit val Equal: Derived[Equal] =
    new ContravariantDerived[Equal] {
      def products[Z](f: Z => ProductX[Equal]): Equal[Z] = { (z1: Z, z2: Z) =>
        ProductX.and(f)(z1, z2).all {
          case (p1, p2) => p1.tc.equal(p1.value, p2.value)
        }
      }

      def coproducts[Z](f: Z => CoproductX[Equal]): Equal[Z] = {
        (z1: Z, z2: Z) =>
          CoproductX.and(f)(z1, z2) match {
            case Just((p1, p2)) => p1.tc.equal(p1.value, p2.value)
            case _              => false
          }
      }
    }

}

trait ContravariantDerived[F[_]]
    extends Derived[F]
    with CodivideX[F]
    with DivisibleX[F] {

  def coproducts[Z](f: Z => CoproductX[F]): F[Z]
  def products[Z](f: Z => ProductX[F]): F[Z]

  final def codivideX[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = coproducts { z =>
    val co = f(z)
    val tc = tcs.values(co.index).asInstanceOf[Name[F[scala.Any]]]
    val v  = co.value
    CoproductX(co.index, ParamX(v, tc.value))
  }

  final def divideX[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = products { z =>
    ProductX(
      IList.fromList((f(z).values zip tcs.values).map { tcv =>
        ParamX(tcv._1, tcv._2.asInstanceOf[Name[F[scala.Any]]].value)
      }(scala.collection.breakOut))
    )
  }

}
object ContravariantDerived {
  @inline def apply[F[_]](
    implicit i: ContravariantDerived[F]
  ): ContravariantDerived[F] = i
}

trait CovariantDerived[F[_]]
    extends Derived[F]
    with CoapplicativeX[F]
    with ApplicativeX[F] {

  //def coproducts[Z](f: CoproductX[F] => Z): F[Z]
  //def products[Z](f: ProductX[F] => Z): F[Z]

  /*
  final def coapplyX[A, Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = scala.Predef.???

  final def applyX[A, Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Prod[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = scala.Predef.???
 */

}
object CovariantDerived {
  @inline def apply[F[_]](
    implicit i: CovariantDerived[F]
  ): CovariantDerived[F] = i
}
