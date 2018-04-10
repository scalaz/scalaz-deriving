// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

import shapeless._
import shapeless.labelled._

import shapeless.{ :+:, Coproduct }

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
object DerivedXDecoder
    extends LowPriorityDerivedXDecoder1
    with LowPriorityDerivedXDecoder2
    with LowPriorityDerivedXDecoder3 {
  def gen[T, R](
    implicit
    G: LabelledGeneric.Aux[T, R],
    LER: Cached[Strict[DerivedXDecoder[R]]],
    T: Typeable[T]
  ): XDecoder[T] = { x =>
    val input = x match {
      case t: XTag      => \/-(t)
      case XChildren(c) => -\/(c)
      case _            => -\/(IList.empty[XTag]) // ignores unexpected
    }
    LER.value.value
      .from(input)
      .map(G.from)
      .leftMap(reason => s"${T.describe} -> $reason")
  }

  trait PXDecoder[R] extends DerivedXDecoder[R]
  trait CXDecoder[R] extends DerivedXDecoder[R] {
    def from(x: IList[XTag] \/ XTag): Out[R] = x match {
      case \/-(tag)                => coproduct(tag)
      case -\/(ICons(tag, INil())) => coproduct(tag)
      case -\/(list)               => -\/(s"unexpected ${list.take(100)}...")
    }
    def coproduct(x: XTag): Out[R]
  }

  implicit val hnil: PXDecoder[HNil] = _ => \/-(HNil)

  implicit def hconsAttr[K <: Symbol, A, AS <: HList](
    implicit K: Witness.Aux[K],
    DV: XDecoder[A @@ XAttribute],
    DR: PXDecoder[AS]
  ): PXDecoder[FieldType[K, A @@ XAttribute] :: AS] = { in =>
    val key = XAtom(K.value.name)

    in.|> {
      case \/-(XTag(_, attrs, _, _)) =>
        val value = attrs.find(_.name == key).map(_.value)
        DV.fromXml(value.getOrElse(XChildren(IList.empty))).leftMap { err =>
          if (value.isEmpty) s"missing attribute ${key.text} from $attrs"
          else err
        }

      case -\/(other) =>
        DV.fromXml(XChildren(IList.empty))
          .leftMap(
            _ =>
              s"missing attribute ${key.text}, got ${other.toString.take(100)}"
          )
    }.flatMap { head =>
      DR.from(in).map { tail =>
        field[K](head) :: tail
      }
    }
  }

  // XML is ambiguous in its representation of single element lists, i.e. we
  // could get a single element unattributed or an XTag. If there was no
  // ambiguity we would want to look at the children of the XTag, but due to the
  // ambiguity we must search in the outer which will leave us at the wrong
  // level in the case of nested single elements of the same name.
  protected def flatChildren(in: IList[XTag] \/ XTag): IList[XTag] = in match {
    case -\/(unattributed) => unattributed
    case \/-(outer @ XTag(_, _, children, _)) =>
      outer :: children
    case \/-(tag) => IList.single(tag) // string body
  }

  implicit def hconsInlinedField[K <: Symbol, A, AS <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[AS]
  ): PXDecoder[FieldType[K, A @@ XInlinedField] :: AS] = { in =>
    val key = XAtom(K.value.name)
    val values = flatChildren(in).filter(_.name == key) match {
      case ICons(value, INil()) => value: XNode
      case list                 => XChildren(list)
    }
    LDV.value.fromXml(values).flatMap { head =>
      DR.from(in).map { tail =>
        field[K](XInlinedField(head)) :: tail
      }
    }
  }

  implicit def hconsInlinedContent[K <: Symbol, A, AS <: HList](
    implicit
    @unused K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[AS]
  ): PXDecoder[FieldType[K, A @@ XInlinedContent] :: AS] = { in =>
    in.|> {
      case -\/(tags) => LDV.value.fromXml(XChildren(tags))
      case \/-(t)    => XDecoder.fromXTagContent(t)(LDV.value)
    }.flatMap { head =>
      DR.from(in).map { tail =>
        field[K](XInlinedContent(head)) :: tail
      }
    }
  }

  implicit val cnil: CXDecoder[CNil] = tag =>
    -\/(s"no valid typehint in '$tag'")
  implicit def ccons[K <: Symbol, V, AS <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LDI: Lazy[XDecoder[V]],
    DR: CXDecoder[AS]
  ): CXDecoder[FieldType[K, V] :+: AS] = {
    case t @ XTag(XAtom(key), INil(), _, _) if key == K.value.name =>
      XDecoder.fromXTagContent(t)(LDI.value).map(a => Inl(field[K](a)))
    case t @ XTag(XAtom(key), _, _, _) if key == K.value.name =>
      LDI.value.fromXml(t).map(a => Inl(field[K](a)))
    case other =>
      DR.coproduct(other).map(a => Inr(a))
  }

}

trait LowPriorityDerivedXDecoder1 {
  this: DerivedXDecoder.type =>

  // Workaround two shapeless bugs:
  //
  // - https://github.com/milessabin/shapeless/issues/309
  // - if LDV is Lazy, it fails to compile
  //
  // so this explicit support has been added.
  implicit def hconsInlinedList[K <: Symbol, A, AS <: HList](
    implicit K: Witness.Aux[K],
    LDV: XDecoder[A @@ XInlinedList],
    DR: PXDecoder[AS]
  ): PXDecoder[
    FieldType[K, A @@ XInlinedList @@ XInlinedField] :: AS
  ] =
    hconsInlinedField(K, Lazy(LDV), DR)

}

trait LowPriorityDerivedXDecoder2 {
  this: DerivedXDecoder.type =>

  implicit def hcons[K <: Symbol, A, AS <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[AS]
  ): PXDecoder[FieldType[K, A] :: AS] = { in =>
    val key = XAtom(K.value.name)

    flatChildren(in)
      .filter(_.name == key)
      .|> {
        case ICons(v, INil()) =>
          XDecoder.fromXTagContent(v)(LDV.value)
        case INil() =>
          LDV.value.fromXml(XChildren(IList.empty)).leftMap { _ =>
            s"missing tag '${key.text}'"
          }
        case list =>
          -\/(s"unexpected ${list.length} elements named '${key.text}'")
      }
      .flatMap { head =>
        DR.from(in).map { tail =>
          field[K](head) :: tail
        }
      }
  }
}

trait LowPriorityDerivedXDecoder3 {
  this: DerivedXDecoder.type =>

  // WORKAROUND https://github.com/milessabin/shapeless/issues/309
  implicit def hconsHack[K <: Symbol, A, T, AS <: HList](
    implicit K: Witness.Aux[K],
    DV: XDecoder[A @@ T],
    DR: PXDecoder[AS]
  ): PXDecoder[FieldType[K, A @@ T] :: AS] = hcons(K, DV, DR)

}
