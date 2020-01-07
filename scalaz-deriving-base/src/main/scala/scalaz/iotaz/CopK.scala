// Copyright: 2017 - 2020 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

// Derived from https://github.com/frees-io/iota
//
// Copyright (C) 2017-2018 Andy Scott.
// Copyright (c) 2017-2018 47 Degrees. <http://47deg.com>
// All rights reserved.
//
// https://github.com/frees-io/iota/blob/v0.3.10/LICENSE
// https://github.com/frees-io/iota/blob/v0.3.10/NOTICE

package scalaz.iotaz

import scala._, Predef._
import scalaz.~>

/** A coproduct of type constructors captured by type constructor list `L` */
final class CopK[LL <: TListK, A] private (
  val index: Int,
  val value: Any
) extends Serializable {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: CopK[LL, A] => (index == other.index) && (value == other.value)
    case _                  => false
  }

  override def hashCode(): Int =
    41 * index + value.##

  override def toString: String =
    s"CopK($value @ $index)"
}

object CopK {

  def unsafeApply[L <: TListK, F[_], A](index: Int, fa: F[A]): CopK[L, A] =
    new CopK[L, A](index, fa)

  /** A type class witnessing the ability to inject type constructor `F`
   * into a coproduct of type constructors `G`
   */
  sealed abstract class Inject[F[_], G[α] <: CopK[_, α]] {
    def inj: F ~> G
    def prj: G ~> λ[α => Option[F[α]]]
    final def apply[A](fa: F[A]): G[A]           = inj(fa)
    final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
  }

  object Inject {
    def apply[F[_], G[α] <: CopK[_, α]](
      implicit ev: Inject[F, G]
    ): Inject[F, G] = ev

    implicit def injectFromInjectL[F[_], L <: TListK](
      implicit ev: InjectL[F, L]
    ): Inject[F, CopK[L, ?]] = new Inject[F, CopK[L, ?]] {
      val inj: F ~> CopK[L, ?] = λ[F ~> CopK[L, ?]](ev.inj(_))
      val prj: CopK[L, ?] ~> λ[α => Option[F[α]]] =
        λ[CopK[L, ?] ~> λ[α => Option[F[α]]]](ev.proj(_))
    }
  }

  /** A type class witnessing the ability to inject type constructor `F`
   * into a coproduct of types constructors for [[TListK]] type `L`
   */
  final class InjectL[F[_], L <: TListK] private[InjectL] (index: Int) {
    def inj[A](fa: F[A]): CopK[L, A] = new CopK[L, A](index, fa)
    def proj[A](ca: CopK[L, A]): Option[F[A]] =
      if (ca.index == index) Some(ca.value.asInstanceOf[F[A]])
      else None
    def apply[A](fa: F[A]): CopK[L, A]           = inj(fa)
    def unapply[A](ca: CopK[L, A]): Option[F[A]] = proj(ca)
  }

  object InjectL {
    def apply[F[_], L <: TListK](implicit ev: InjectL[F, L]): InjectL[F, L] = ev
    implicit def makeInjectL[F[_], L <: TListK](
      implicit ev: TListK.Pos[L, F]
    ): InjectL[F, L] =
      new InjectL[F, L](ev.index)
  }

  final class RemoveL[F[_], L <: TListK] private[RemoveL] (index: Int) {
    def apply[A](c: CopK[L, A]): Either[CopK[TListK.Op.Remove[F, L], A], F[A]] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[F[A]],
        new CopK(if (c.index < index) c.index else c.index - 1, c.value)
      )
  }

  object RemoveL {
    def apply[F[_], L <: TListK](implicit ev: RemoveL[F, L]): RemoveL[F, L] = ev
    implicit def makeRemoveL[F[_], L <: TListK](
      implicit ev: TListK.Pos[L, F]
    ): RemoveL[F, L] =
      new RemoveL[F, L](ev.index)
  }

  val NaturalTransformation: CopKNaturalTransformation.type =
    CopKNaturalTransformation
}
