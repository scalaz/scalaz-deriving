// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.typeclasses

import simulacrum.typeclass

@typeclass trait Cofoo[T] {}
trait DerivedCofoo[T] extends Cofoo[T]
object DerivedCofoo {
  def gen[T, Repr](
    implicit G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): DerivedCofoo[T] = new DerivedCofoo[T] {}
}

@typeclass trait Cobar[T] {}
object CustomCobar {
  def go[T, Repr](
    implicit G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): Cobar[T] = new Cobar[T] {}
}

package a {
  @typeclass trait Cobaz[T[_]] {}
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T] {}
  object DerivedCobaz {
    def gen[T[_]](
      implicit G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null
  }
}
package b {
  @typeclass trait Cobaz[T[_]] {}
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T] {}
  object DerivedCobaz {
    def gen[T[_]](
      implicit G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null
  }
}
