// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

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
      def products[C[_]: Foldable, Z](params: C[Param[Z, Equal]]): Equal[Z] = {
        (z1: Z, z2: Z) =>
          params.all { f =>
            f.tc.equal(f(z1), f(z2))
          }
      }

      def coproducts[C[_]: Foldable1, Z](
        params: C[Coparam[Z, Equal]]
      ): Equal[Z] = { (z1: Z, z2: Z) =>
        params.any { f =>
          (f(z1), f(z2)) match {
            case (Just(a), Just(b)) => f.tc.equal(a, b)
            case _                  => false
          }
        }
      }
    }

}

// TODO: does the fight for .contramap break any laws?
trait ContravariantTypeclassDerivation[F[_]]
    extends TypeclassDerivation[F]
    with CodivideX[F]
    with DivisibleX[F] // DivisibleX.contramap wins
    {
  final def codivideX[C[_]: Foldable1, Z](params: C[Coparam[Z, F]]): F[Z] =
    coproducts(params)
  def coproducts[C[_]: Foldable1, Z](params: C[Coparam[Z, F]]): F[Z]

  final def divideX[C[_]: Foldable, Z](params: C[Param[Z, F]]): F[Z] =
    products(params)
  def products[C[_]: Foldable, Z](params: C[Param[Z, F]]): F[Z]
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
