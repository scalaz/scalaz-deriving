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

  /*
  implicit val derivation: TypeclassDerivation[Default] =
    new CovariantTypeclassDerivation[Default] {
      def products[Z](f: ProductX[Default] => Z): Default[Z] = scala.Predef.???
      def coproducts[Z](f: CoproductX[Default] => Z): Default[Z] = {
        val cop = Coproduct

        instance {
          f {
          }
        }
     }

    }
   */

  /*
  implicit val derivation
    : LazyApplicative[Default] with Coapplicative[Default] =
    new LazyApplicative[Default] with Coapplicative[Default]
    with DangerousApplicative[Default] {
      override def point[Z](z: => Z): Default[Z] = instance(z)

      override def apply2[A1, A2, Z](a1: => Default[A1], a2: => Default[A2])(
        f: (A1, A2) => Z
      ): Default[Z] = instance(f(a1.default, a2.default))

      def coapply2[Z, A1, A2](a1: => Default[A1], a2: => Default[A2])(
        f: A1 \/ A2 => Z
      ): Default[Z] = instance(f(-\/(a1.default)))
    }
   */

  implicit val derivation: CoapplicativeX[Default] =
    new CoapplicativeX[Default] {

      import iotaz._
      import iotaz.TList.Compute.{ Aux => ↦ }
      import iotaz.TList.Op.{ Map => ƒ }

      def coapplyX[Z, L <: TList, TL <: TList](
        tcs: Prod[TL]
      )(
        f: Cop[L] => Z
      )(
        implicit
        ev: λ[a => Name[Default[a]]] ƒ L ↦ TL
      ): Default[Z] = instance {
        scala.Predef.???
        /*        val cop = getter(tcs) { (d: Default[_]) =>
          Maybe.just(d.default)
        }.getOrElse {
          null // mapping over the Foldable1 means we wouldn't need this
        }
        f(cop)*/
      }

      // TODO: a way of going from an element in the tc sequence to
      //       the coproduct by providing a callback.
      // TODO: then generalise to a list over the Prod

      // TODO: generalise Int to a findFirst, or maybe f returns an Option for
      //       the selection (which means we need a fallback).

//      import scala.collection.immutable.Seq

      // def getter[Y, L <: TList, TL <: TList](
      //   tcs: Prod[TL]
      // )(f: Default[_] => Maybe[Y])(
      //   implicit
      //   ev1: λ[a => Name[Default[a]]] ƒ L ↦ TL,
      //   ev2: Cop.InjectL[Y, L]
      // ): Maybe[Cop[L]] = Maybe.fromOption {
      //   tcs.values
      //     .asInstanceOf[Seq[Name[Default[_]]]]
      //     .zipWithIndex
      //     .toStream
      //     .flatMap {
      //       case (v, i) =>
      //         f(v.value).toOption.map { y =>
      //           ev2.inj(y)
      //         }
      //     }
      //     .headOption
      // }

    }

}
