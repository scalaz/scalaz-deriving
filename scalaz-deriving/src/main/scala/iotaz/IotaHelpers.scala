// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package iotaz

import java.lang.String

import scala.annotation.switch
import scala.collection.immutable.List

import TList._
import TList.Compute.{ Aux => ↦ }
import TList.Op.{ Map => ƒ }

import scalaz._
import Isomorphism.IsoSet

final class ProdGen[A, Repr <: TList, Labels <: TList] private (
  val from: A => Prod[Repr],
  val to: Prod[Repr] => A,
  val labels: Prod[Labels]
) extends IsoSet[Prod[Repr], A]
object ProdGen {
  def apply[A, R <: TList, L <: TList](
    f: A => Prod[R],
    t: Prod[R] => A,
    n: Prod[L]
  )(
    implicit @unused ev: λ[a => String] ƒ R ↦ L
  ): ProdGen[A, R, L] = new ProdGen(f, t, n)

  def gen[A, R <: TList, L <: TList]: ProdGen[A, R, L] =
    macro IotaMacros.prodGen[A, R, L]
}

final class CopGen[A, Repr <: TList, Labels <: TList] private (
  val from: A => Cop[Repr],
  val to: Cop[Repr] => A,
  val labels: Prod[Labels]
) extends IsoSet[Cop[Repr], A]
object CopGen {
  def apply[A, R <: TList, L <: TList](
    f: A => Cop[R],
    t: Cop[R] => A,
    n: Prod[L]
  )(
    implicit @unused ev: λ[a => String] ƒ R ↦ L
  ): CopGen[A, R, L] = new CopGen(f, t, n)

  def gen[A, R <: TList, L <: TList]: CopGen[A, R, L] =
    macro IotaMacros.copGen[A, R, L]
}

// unintentional joke about the state of northern irish politics...
object LazyProd {
  def apply[A1](a1: =>A1): Prod[Name[A1] :: TNil] = Prod(Need(a1))
  def apply[A1, A2](a1: =>A1, a2: =>A2): Prod[Name[A1] :: Name[A2] :: TNil] =
    Prod(Need(a1), Need(a2))
  def apply[A1, A2, A3](
    a1: =>A1,
    a2: =>A2,
    a3: =>A3
  ): Prod[Name[A1] :: Name[A2] :: Name[A3] :: TNil] =
    Prod(Need(a1), Need(a2), Need(a3))
  def apply[A1, A2, A3, A4](
    a1: =>A1,
    a2: =>A2,
    a3: =>A3,
    a4: =>A4
  ): Prod[Name[A1] :: Name[A2] :: Name[A3] :: Name[A4] :: TNil] =
    Prod(Need(a1), Need(a2), Need(a3), Need(a4))
}

object Prods {

  val empty: Prod[TNil] = Prod()
  def from1T[A1](e: A1): Prod[A1 :: TNil] =
    Prod.unsafeApply(List(e))
  def from2T[A1, A2](e: (A1, A2)): Prod[A1 :: A2 :: TNil] =
    Prod.unsafeApply(List(e._1, e._2))
  def from3T[A1, A2, A3](e: (A1, A2, A3)): Prod[A1 :: A2 :: A3 :: TNil] =
    Prod.unsafeApply(List(e._1, e._2, e._3))
  def from4T[A1, A2, A3, A4](
    e: (A1, A2, A3, A4)
  ): Prod[A1 :: A2 :: A3 :: A4 :: TNil] =
    Prod.unsafeApply(List(e._1, e._2, e._3, e._4))

  def to1T[A1](a: Prod[A1 :: TNil]): A1 = a.values(0).asInstanceOf[A1]
  def to2T[A1, A2](a: Prod[A1 :: A2 :: TNil]): (A1, A2) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2]
  )
  def to3T[A1, A2, A3](a: Prod[A1 :: A2 :: A3 :: TNil]): (A1, A2, A3) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3]
  )
  def to4T[A1, A2, A3, A4](
    a: Prod[A1 :: A2 :: A3 :: A4 :: TNil]
  ): (A1, A2, A3, A4) = (
    a.values(0).asInstanceOf[A1],
    a.values(1).asInstanceOf[A2],
    a.values(2).asInstanceOf[A3],
    a.values(3).asInstanceOf[A4]
  )

}

object Cops {
  def from1[A1](e: A1): Cop[A1 :: TNil] = Cop.unsafeApply(0, e)
  def from2[A1, A2](e: A1 \/ A2): Cop[A1 :: A2 :: TNil] = e match {
    case -\/(a) => Cop.unsafeApply(0, a)
    case \/-(b) => Cop.unsafeApply(1, b)
  }
  def from3[A1, A2, A3](e: A1 \/ (A2 \/ A3)): Cop[A1 :: A2 :: A3 :: TNil] =
    e match {
      case -\/(a)      => Cop.unsafeApply(0, a)
      case \/-(-\/(b)) => Cop.unsafeApply(1, b)
      case \/-(\/-(c)) => Cop.unsafeApply(2, c)
    }
  def from4[A1, A2, A3, A4](
    e: A1 \/ (A2 \/ (A3 \/ A4))
  ): Cop[A1 :: A2 :: A3 :: A4 :: TNil] =
    e match {
      case -\/(a)           => Cop.unsafeApply(0, a)
      case \/-(-\/(b))      => Cop.unsafeApply(1, b)
      case \/-(\/-(-\/(c))) => Cop.unsafeApply(2, c)
      case \/-(\/-(\/-(d))) => Cop.unsafeApply(3, d)
    }

  def to1[A1](c: Cop[A1 :: TNil]): A1 = c.value.asInstanceOf[A1]
  def to2[A1, A2](c: Cop[A1 :: A2 :: TNil]): A1 \/ A2 =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(c.value.asInstanceOf[A2])
    }
  def to3[A1, A2, A3](c: Cop[A1 :: A2 :: A3 :: TNil]): A1 \/ (A2 \/ A3) =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
      case 2 => \/-(\/-(c.value.asInstanceOf[A3]))
    }
  def to4[A1, A2, A3, A4](
    c: Cop[A1 :: A2 :: A3 :: A4 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ A4)) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(c.value.asInstanceOf[A4])))
  }

}
