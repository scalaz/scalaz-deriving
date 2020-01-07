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

/** A coproduct of types captured by type list `L` */
final class Cop[LL <: TList] private (
  val index: Int,
  val value: Any
) extends Serializable {
  type L = LL

  override def equals(anyOther: Any): Boolean = anyOther match {
    case other: Cop[LL] => (index == other.index) && (value == other.value)
    case _              => false
  }

  override def hashCode(): Int =
    41 * index + value.##

  override def toString: String =
    s"Cop($value @ $index)"
}

object Cop {

  import scalaz.Isomorphism._
  def gen[A, R <: TList]: A <=> Cop[R] = macro internal.CopMacros.copGen[A, R]

  def unsafeApply[L <: TList, A](index: Int, a: A): Cop[L] =
    new Cop[L](index, a)

  /** A type class witnessing the ability to inject type `A` into a
   * coproduct of types `B`
   */
  sealed abstract class Inject[A, B <: Cop[_]] {
    def inj: A => B
    def prj: B => Option[A]
    final def apply(a: A): B           = inj(a)
    final def unapply(b: B): Option[A] = prj(b)
  }

  object Inject {
    def apply[A, B <: Cop[_]](implicit ev: Inject[A, B]): Inject[A, B] = ev

    implicit def injectFromInjectL[A, L <: TList](
      implicit ev: InjectL[A, L]
    ): Inject[A, Cop[L]] = new Inject[A, Cop[L]] {
      val inj: A => Cop[L]         = ev.inj(_)
      val prj: Cop[L] => Option[A] = ev.proj(_)
    }
  }

  /** A type class witnessing the ability to inject type `A` into a
   * coproduct of types for [[TList]] type `L`
   */
  final class InjectL[A, L <: TList] private[InjectL] (index: Int) {
    def inj(a: A): Cop[L] = new Cop[L](index, a)
    def proj(c: Cop[L]): Option[A] =
      if (c.index == index) Some(c.value.asInstanceOf[A])
      else None
    def apply(a: A): Cop[L]           = inj(a)
    def unapply(c: Cop[L]): Option[A] = proj(c)
  }

  object InjectL {
    def apply[A, L <: TList](implicit ev: InjectL[A, L]): InjectL[A, L] = ev
    implicit def makeInjectL[A, L <: TList](
      implicit ev: TList.Pos[L, A]
    ): InjectL[A, L] =
      new InjectL[A, L](ev.index)
  }

  final class RemoveL[A, L <: TList] private[RemoveL] (index: Int) {
    def apply(c: Cop[L]): Either[Cop[TList.Op.Remove[A, L]], A] =
      Either.cond(
        c.index == index,
        c.value.asInstanceOf[A],
        new Cop(if (c.index < index) c.index else c.index - 1, c.value)
      )
  }

  object RemoveL {
    def apply[A, L <: TList](implicit ev: RemoveL[A, L]): RemoveL[A, L] = ev
    implicit def makeRemoveL[A, L <: TList](
      implicit ev: TList.Pos[L, A]
    ): RemoveL[A, L] =
      new RemoveL[A, L](ev.index)
  }

}
