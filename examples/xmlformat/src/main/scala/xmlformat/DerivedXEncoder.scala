// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{ -\/, \/, \/-, ICons, IList, INil }
import scalaz.Scalaz._

import shapeless._
import shapeless.labelled._
import xmlformat.{ XAttr, XString, XTag }

sealed trait DerivedXEncoder[R, AS <: HList, BS <: HList] {
  // a union type would be very nice here... left is for products, right is for
  // coproducts. a left tag in a coproduct means the name can be replaced, a
  // right tag in a coproduct means it should be retained.
  private[generic] def to(
    r: R,
    as: AS,
    bs: BS
  ): IList[XAttr \/ (XTag \/ XString)] \/ (XTag \/ XTag)
}
object DerivedXEncoder {

  def gen[A, R, AS <: HList, BS <: HList](
    implicit
    T: Typeable[A],
    G: LabelledGeneric.Aux[A, R],
    AA: Annotations.Aux[x.attr, A, AS],
    AB: Annotations.Aux[x.body, A, BS],
    R: Cached[Strict[DerivedXEncoder[R, AS, BS]]]
  ): XEncoder[A] = { t =>
    R.value.value.to(G.to(t), AA(), AB()) match {
      case -\/(product) =>
        val (attrs, content) = product.separate
        val (children, body) = content.separate
        XTag(T.describe, attrs, children, body.fold1Opt.toMaybe).asChild
      case \/-(coproduct) =>
        coproduct.leftMap(_.copy(name = T.describe)).merge.asChild
    }
  }

  sealed trait PXEncoder[R, AS <: HList, BS <: HList]
      extends DerivedXEncoder[R, AS, BS] {
    final def to(
      r: R,
      as: AS,
      bs: BS
    ): IList[XAttr \/ (XTag \/ XString)] \/ (XTag \/ XTag) =
      -\/(product(r, as, bs))
    def product(r: R, as: AS, bs: BS): IList[XAttr \/ (XTag \/ XString)]

    protected final def lift(a: XAttr): XAttr \/ (XTag \/ XString) = -\/(a)
    protected final def lift(a: XTag): XAttr \/ (XTag \/ XString)  = \/-(-\/(a))
    protected final def lift(a: XString): XAttr \/ (XTag \/ XString) =
      \/-(\/-(a))
  }

  implicit val hnil: PXEncoder[HNil, HNil, HNil] =
    new PXEncoder[HNil, HNil, HNil] {
      def product(
        r: HNil,
        as: HNil,
        bs: HNil
      ): IList[XAttr \/ (XTag \/ XString)] = IList.empty
    }

  implicit def hcons[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XEncoder[H]],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] =
    new PXEncoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] {
      def product(
        r: FieldType[K, H] :: T,
        as: None.type :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = r match {
        case head :: tail =>
          H.value.toXml(head).tree.map {
            case XTag(_, as, ts, bd) => lift(XTag(K.value.name, as, ts, bd))
          } ::: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsOptional[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XEncoder[H]],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, Option[H]] :: T,
    None.type :: AS,
    None.type :: BS
  ] =
    new PXEncoder[
      FieldType[K, Option[H]] :: T,
      None.type :: AS,
      None.type :: BS
    ] {
      def product(
        r: FieldType[K, Option[H]] :: T,
        as: None.type :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] =
        (r: Option[H] :: T) match {
          case None :: tail => T.product(tail, as.tail, bs.tail)
          case Some(head) :: tail =>
            H.value.toXml(head).tree.map {
              case XTag(_, as, ts, bd) => lift(XTag(K.value.name, as, ts, bd))
            } ::: T.product(tail, as.tail, bs.tail)
        }
    }

  implicit def hconsStr[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] =
    new PXEncoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] {
      def product(
        r: FieldType[K, H] :: T,
        as: None.type :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = r match {
        case head :: tail =>
          val tag = XTag(K.value.name, H.toXml(head))
          lift(tag) :: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsStrOptional[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, Option[H]] :: T,
    None.type :: AS,
    None.type :: BS
  ] =
    new PXEncoder[
      FieldType[K, Option[H]] :: T,
      None.type :: AS,
      None.type :: BS
    ] {
      def product(
        r: FieldType[K, Option[H]] :: T,
        as: None.type :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = (r: Option[H] :: T) match {
        case None :: tail => T.product(tail, as.tail, bs.tail)
        case Some(head) :: tail =>
          val tag = XTag(K.value.name, H.toXml(head))
          lift(tag) :: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsAttr[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, H] :: T,
    Some[x.attr] :: AS,
    None.type :: BS
  ] =
    new PXEncoder[
      FieldType[K, H] :: T,
      Some[x.attr] :: AS,
      None.type :: BS
    ] {
      def product(
        r: FieldType[K, H] :: T,
        as: Some[x.attr] :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = r match {
        case head :: tail =>
          val key   = K.value.name
          val value = H.toXml(head)
          lift(XAttr(key, value)) :: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsAttrOptional[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, Option[H]] :: T,
    Some[x.attr] :: AS,
    None.type :: BS
  ] =
    new PXEncoder[
      FieldType[K, Option[H]] :: T,
      Some[x.attr] :: AS,
      None.type :: BS
    ] {
      def product(
        r: FieldType[K, Option[H]] :: T,
        as: Some[x.attr] :: AS,
        bs: None.type :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = (r: Option[H] :: T) match {
        case None :: tail => T.product(tail, as.tail, bs.tail)
        case Some(head) :: tail =>
          val key   = K.value.name
          val value = H.toXml(head)
          lift(XAttr(key, value)) :: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsInlinedField[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    H: Lazy[XEncoder[H]],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, H] :: T,
    None.type :: AS,
    Some[x.body] :: BS
  ] =
    new PXEncoder[
      FieldType[K, H] :: T,
      None.type :: AS,
      Some[x.body] :: BS
    ] {
      def product(
        r: FieldType[K, H] :: T,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = r match {
        case head :: tail =>
          H.value
            .toXml(head)
            .tree
            .map(lift) ::: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsInlinedStr[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, H] :: T,
    None.type :: AS,
    Some[x.body] :: BS
  ] =
    new PXEncoder[
      FieldType[K, H] :: T,
      None.type :: AS,
      Some[x.body] :: BS
    ] {
      def product(
        r: FieldType[K, H] :: T,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = r match {
        case head :: tail =>
          lift(H.toXml(head)) :: T.product(tail, as.tail, bs.tail)
      }
    }

  implicit def hconsInlinedStrOptional[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    H: XStrEncoder[H],
    T: PXEncoder[T, AS, BS]
  ): PXEncoder[
    FieldType[K, Option[H]] :: T,
    None.type :: AS,
    Some[x.body] :: BS
  ] =
    new PXEncoder[
      FieldType[K, Option[H]] :: T,
      None.type :: AS,
      Some[x.body] :: BS
    ] {
      def product(
        r: FieldType[K, Option[H]] :: T,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): IList[XAttr \/ (XTag \/ XString)] = (r: Option[H] :: T) match {
        case None :: tail => T.product(tail, as.tail, bs.tail)
        case Some(head) :: tail =>
          lift(H.toXml(head)) :: T.product(tail, as.tail, bs.tail)
      }
    }

  sealed trait CXEncoder[R, AS <: HList, BS <: HList]
      extends DerivedXEncoder[R, AS, BS] {
    final def to(
      r: R,
      as: AS,
      bs: BS
    ): IList[XAttr \/ (XTag \/ XString)] \/ (XTag \/ XTag) =
      \/-(coproduct(r, as, bs))
    def coproduct(r: R, as: AS, bs: BS): XTag \/ XTag
  }

  implicit val cnil: CXEncoder[CNil, HNil, HNil] =
    new CXEncoder[CNil, HNil, HNil] {
      def coproduct(r: CNil, as: HNil, bs: HNil): Nothing =
        sys.error("impossible")
    }

  implicit def ccons[K <: Symbol, H, T <: Coproduct, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XNodeEncoder[H]],
    T: CXEncoder[T, AS, BS]
  ): CXEncoder[
    FieldType[K, H] :+: T,
    None.type :: AS,
    None.type :: BS
  ] =
    new CXEncoder[
      FieldType[K, H] :+: T,
      None.type :: AS,
      None.type :: BS
    ] {
      private val hint = XAttr("typehint", XString(K.value.name))
      def coproduct(
        r: FieldType[K, H] :+: T,
        as: None.type :: AS,
        bs: None.type :: BS
      ): XTag \/ XTag = r match {
        case Inl(ins) =>
          H.value.toXml(ins) match {
            case XChildren(ICons(XTag(n, as, ts, b), INil())) =>
              XTag(n, hint :: as, ts, b).left
            case c =>
              XTag("value", c).copy(attrs = IList.single(hint)).left
          }

        case Inr(rem) => T.coproduct(rem, as.tail, bs.tail)
      }
    }

  implicit def cconsNodeTag[
    K <: Symbol,
    H,
    T <: Coproduct,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XNodeEncoder[H]],
    T: CXEncoder[T, AS, BS]
  ): CXEncoder[
    FieldType[K, H] :+: T,
    None.type :: AS,
    Some[x.body] :: BS
  ] =
    new CXEncoder[
      FieldType[K, H] :+: T,
      None.type :: AS,
      Some[x.body] :: BS
    ] {
      private val key = K.value.name
      def coproduct(
        r: FieldType[K, H] :+: T,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): XTag \/ XTag = r match {
        case Inl(ins) =>
          H.value.toXml(ins) match {
            case XChildren(ICons(t: XTag, INil())) => t.copy(name = key).right
            case c                                 => XTag(key, c).right
          }
        case Inr(rem) => T.coproduct(rem, as.tail, bs.tail)
      }
    }

}
