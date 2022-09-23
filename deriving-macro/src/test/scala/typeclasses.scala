// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package testing.typeclasses

import java.lang.String

import scala.Either

import scalaz.unused

import simulacrum.typeclass

@typeclass trait Cofoo[A] {
  def toFoo(@unused a: A): String =
    "this is the default gen codepath"

  final def xmap[B](@unused f: A => B, @unused g: B => A): Cofoo[B] =
    new Cofoo[B] {
      override def toFoo(b: B): String = "exercised the xmap codepath"
    }
}
object Cofoo              {
  implicit val string: Cofoo[String]                           = new Cofoo[String] {}
  implicit def either[L: Cofoo, R: Cofoo]: Cofoo[Either[L, R]] =
    new Cofoo[Either[L, R]] {}
}
trait DerivedCofoo[T] extends Cofoo[T]
object DerivedCofoo       {
  def gen[T, Repr](implicit
    @unused G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): DerivedCofoo[T] = new DerivedCofoo[T] {}
}

@typeclass trait Cobar[T] {}
object Cobar              {
  implicit val invariant: scalaz.InvariantFunctor[Cobar] =
    new scalaz.InvariantFunctor[Cobar] {
      override def xmap[A, B](ma: Cobar[A], f: A => B, g: B => A): Cobar[B] =
        new Cobar[B] {}
    }

  implicit val string: Cobar[String] = new Cobar[String] {}
}

@typeclass trait OrphanCobar[T] {}
object OrphanCobarInstances     {
  implicit val invariant: scalaz.InvariantFunctor[OrphanCobar] =
    new scalaz.InvariantFunctor[OrphanCobar] {
      override def xmap[A, B](
        ma: OrphanCobar[A],
        f: A => B,
        g: B => A
      ): OrphanCobar[B] =
        new OrphanCobar[B] {}
    }

  implicit val string: OrphanCobar[String] = new OrphanCobar[String] {}
}
object CustomCobar              {
  def go[T, Repr](implicit
    @unused G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): Cobar[T] = new Cobar[T] {}
}

@typeclass trait CustomGen[T] {}
object CustomGen              {
  def gen[T]: CustomGen[T] = new CustomGen[T] {}
}

package a {
  @typeclass trait Cobaz[T[_]] {}
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T] {}
  object DerivedCobaz          {
    def gen[T[_]](implicit
      @unused G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null /* scalafix:ok */
  }
}
package b {
  @typeclass trait Cobaz[T[_]] {}
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T] {}
  object DerivedCobaz {
    def gen[T[_]](implicit
      @unused G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null /* scalafix:ok */
  }
}
