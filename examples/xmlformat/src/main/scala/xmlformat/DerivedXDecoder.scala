// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scala.reflect.ClassTag

import scalaz.{ -\/, @@, \/, \/-, ICons, IList, INil }

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

import XDecoder.Out

/**
 * Product and coproduct decoder.
 *
 * The XDecoder returned by gen uses a list of XTags for unattributed products
 * and XTag for coproducts or products with attributes.
 */
sealed trait DerivedXDecoder[R] {
  private[xmlformat] def from(x: IList[XTag] \/ XTag): Out[R]
}
object DerivedXDecoder extends LowPriorityDerivedXDecoder {
  def gen[T, Repr](
    implicit
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedXDecoder[Repr]]],
    CT: ClassTag[T]
  ): XDecoder[T] = { x =>
    val input = x match {
      case t: XTag      => \/-(t)
      case XChildren(c) => -\/(c)
      case _            => -\/(IList.empty[XTag]) // ignores unexpected
    }
    LER.value.value
      .from(input)
      .map(G.from)
      .leftMap(reason => s"when decoding ${CT}: $reason")
  }

  trait PXDecoder[R] extends DerivedXDecoder[R]
  trait CXDecoder[R] extends DerivedXDecoder[R] {
    def from(x: IList[XTag] \/ XTag): Out[R] = x match {
      case \/-(tag)                => coproduct(tag)
      case -\/(ICons(tag, INil())) => coproduct(tag)
      case -\/(list)               => -\/(s"unexpected $list")
    }
    def coproduct(x: XTag): Out[R]
  }

  implicit val hnil: PXDecoder[HNil] = _ => \/-(HNil)
  implicit def hconsAttr[Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    DV: XDecoder[Value @@ XAttribute],
    DR: PXDecoder[Remaining]
  ): PXDecoder[FieldType[Key, Value @@ XAttribute] :*: Remaining] = { in =>
    val key = XAtom(Key.value.name)

    val head = in match {
      case \/-(XTag(_, attrs, _)) =>
        val value = attrs.find(_.name == key).map(_.value)
        DV.fromXml(value.getOrElse(XChildren(IList.empty))).leftMap { err =>
          if (value.isEmpty) s"missing attribute ${key.text}" else err
        }

      case -\/(_) =>
        DV.fromXml(XChildren(IList.empty))
          .leftMap(_ => s"missing attribute ${key.text}")
    }

    head.flatMap { head =>
      DR.from(in).map { tail =>
        field[Key](head) :: tail
      }
    }

  }

  implicit val cnil: CXDecoder[CNil] = tag =>
    -\/(s"no valid typehint in '$tag'")
  implicit def ccons[Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    Name: Witness.Aux[Name],
    LDI: Lazy[XDecoder[Instance]],
    DR: CXDecoder[Remaining]
  ): CXDecoder[FieldType[Name, Instance] :+: Remaining] = {
    case XTag(XAtom(key), INil(), children) if key == Name.value.name =>
      LDI.value.fromXml(children).map(a => Inl(field[Name](a)))
    case tag @ XTag(XAtom(key), _, _) if key == Name.value.name =>
      LDI.value.fromXml(tag).map(a => Inl(field[Name](a)))
    case other =>
      DR.coproduct(other).map(a => Inr(a))
  }

}

trait LowPriorityDerivedXDecoder {
  this: DerivedXDecoder.type =>

  implicit def hcons[Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LDV: Lazy[XDecoder[Value]],
    DR: PXDecoder[Remaining]
  ): PXDecoder[FieldType[Key, Value] :*: Remaining] = { in =>
    val values = in match {
      case -\/(unattributed) => unattributed
      case \/-(outer @ XTag(_, _, XChildren(children))) =>
        outer :: children // ambiguous nesting
      case \/-(tag) => IList.single(tag)
    }

    val key = XAtom(Key.value.name)
    val value = values
      .find(_.name == key)
      .map(_.children)

    LDV.value.fromXml(value.getOrElse(XChildren(IList.empty))) match {
      case -\/(_) if value.isEmpty => -\/(s"missing tag ${key.text}")
      case other =>
        other.flatMap { head =>
          DR.from(in).map { tail =>
            field[Key](head) :: tail
          }
        }
    }
  }
}
