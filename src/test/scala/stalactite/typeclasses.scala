// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.typeclasses

import java.lang.String

import scala.Either

import simulacrum.typeclass

@typeclass trait Cofoo[A] {
  def toFoo(a: A): String = "this is the default gen codepath"

  final def xmap[B](f: A => B, g: B => A): Cofoo[B] = new Cofoo[B] {
    override def toFoo(b: B): String = "exercised the xmap codepath"
  }
}
object Cofoo {
  implicit val string: Cofoo[String] = new Cofoo[String] {}
  implicit def either[L: Cofoo, R: Cofoo]: Cofoo[Either[L, R]] =
    new Cofoo[Either[L, R]] {}
}
trait DerivedCofoo[T] extends Cofoo[T]
object DerivedCofoo {
  def gen[T, Repr](
    implicit G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): DerivedCofoo[T] = new DerivedCofoo[T] {}
}

@typeclass trait Cobar[T] {}
object Cobar {
  implicit val invariant: scalaz.InvariantFunctor[Cobar] =
    new scalaz.InvariantFunctor[Cobar] {
      override def xmap[A, B](ma: Cobar[A], f: A => B, g: B => A): Cobar[B] =
        new Cobar[B] {}
    }

  implicit val string: Cobar[String] = new Cobar[String] {}
}
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
