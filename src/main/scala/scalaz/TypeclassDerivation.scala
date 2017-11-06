// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

// TODO: support arbitrary arity an the invariant level. i.e.
// CoproductiveCodivideX and ApplicativeDivisibleX.
//
// allows typeclass derivation for products, coproducts and AnyVal
trait TypeclassDerivation[F[_]]
    extends CoproductiveCodivide[F]
    with ApplicativeDivisible[F]
object TypeclassDerivation {
  @inline def apply[F[_]](
    implicit i: TypeclassDerivation[F]
  ): TypeclassDerivation[F] = i

  import Scalaz._
  import Maybe.Just

  // should really be on the companion of Equal
  implicit val Equal: TypeclassDerivation[Equal] =
    new ContravariantTypeclassDerivation[Equal] {
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

// TODO: does the fight for .contramap break any laws?
trait ContravariantTypeclassDerivation[F[_]]
    extends TypeclassDerivation[F]
    with CodivideX[F]
    with DivisibleX[F] {
  final def codivideX[Z](f: Z => CoproductX[F]): F[Z] = coproducts(f)
  def coproducts[Z](f: Z => CoproductX[F]): F[Z]

  final def divideX[Z](f: Z => ProductX[F]): F[Z] = products(f)
  def products[Z](f: Z => ProductX[F]): F[Z]

}
object ContravariantTypeclassDerivation {
  @inline def apply[F[_]](
    implicit i: ContravariantTypeclassDerivation[F]
  ): ContravariantTypeclassDerivation[F] = i
}

// TODO: does the fight for .map break any laws?
trait CovariantTypeclassDerivation[F[_]]
    extends TypeclassDerivation[F]
    with CoproductiveX[F]
    with ProductiveX[F] // ProductiveX.map wins
object CovariantTypeclassDerivation {
  @inline def apply[F[_]](
    implicit i: CovariantTypeclassDerivation[F]
  ): CovariantTypeclassDerivation[F] = i
}
