// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{ Coproduct => _, :+: => _, _ }, Scalaz._
import shapeless._
import shapeless.labelled._

import XDecoder.fail
import xmlformat.XAttr
import xmlformat.internal.StringyMultiMap

private[generic] final case class FastXTag(
  x: XTag,
  attrs: StringyMultiMap[XAttr],
  children: StringyMultiMap[XTag]
)
sealed trait DerivedXDecoder[R, AS <: HList, BS <: HList] {
  private[generic] def from(x: XTag, as: AS, bs: BS): String \/ R
}
object DerivedXDecoder extends LowPriorityDerivedXDecoder1 {

  def gen[A, R, AS <: HList, BS <: HList](
    implicit
    G: LabelledGeneric.Aux[A, R],
    AA: Annotations.Aux[x.attr, A, AS],
    AB: Annotations.Aux[x.body, A, BS],
    R: Cached[Strict[DerivedXDecoder[R, AS, BS]]],
    T: Typeable[A]
  ): XDecoder[A] = {
    case XChildren(ICons(t, INil())) =>
      R.value.value
        .from(t, AA(), AB())
        .map(G.from)
        .leftMap(reason => s"${T.describe} -> $reason")

    case got =>
      fail("one tag", got)
        .leftMap(reason => s"${T.describe} -> $reason")
  }

  sealed trait PXDecoder[R, AS <: HList, BS <: HList]
      extends DerivedXDecoder[R, AS, BS] {
    private[generic] def from(x: FastXTag, as: AS, bs: BS): String \/ R

    override final def from(x: XTag, as: AS, bs: BS): String \/ R = {
      // we should get the length of the AS from shapeless here
      val attrs    = StringyMultiMap(x.attrs, 1)(_.name)
      val children = StringyMultiMap(x.children, 1)(_.name)
      from(FastXTag(x, attrs, children), as, bs)
    }
  }
  sealed trait CXDecoder[R, AS <: HList, BS <: HList]
      extends DerivedXDecoder[R, AS, BS]

  implicit val hnil: PXDecoder[HNil, HNil, HNil] =
    new PXDecoder[HNil, HNil, HNil] {
      private val empty                                              = HNil.right[String]
      def from(x: FastXTag, as: HNil, bs: HNil): String \/ HNil.type = empty
    }

  implicit def hconsAttr[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, Some[x.attr] :: AS, None.type :: BS] =
    new PXDecoder[FieldType[K, H] :: T, Some[x.attr] :: AS, None.type :: BS] {
      private val key = K.value.name
      def from(
        in: FastXTag,
        as: Some[x.attr] :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, H] :: T) =
        for {
          head <- in.attrs.find(key) match {
                   case Maybe.Just(attr) => H.fromXml(attr.value)
                   case _                => fail(s"attr '$key'", in.x.asChild)
                 }
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
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
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, Option[H]] :: T, Some[x.attr] :: AS, None.type :: BS] =
    new PXDecoder[FieldType[K, Option[H]] :: T, Some[x.attr] :: AS, None.type :: BS] {
      private val key   = K.value.name
      private val empty = Option.empty[H].right[String]
      def from(
        in: FastXTag,
        as: Some[x.attr] :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, Option[H]] :: T) =
        for {
          head <- in.attrs.find(key) match {
                   case Maybe.Just(attr) => H.fromXml(attr.value).map(Option(_))
                   case _                => empty
                 }
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
    }

  implicit def hconsInlinedStr[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] {
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :: T) =
        for {
          body <- in.x.body \/> fail("a body", in.x.asChild).a
          head <- H.fromXml(body)
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
    }

  implicit def hconsInlinedStrOptional[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, Option[H]] :: T, None.type :: AS, Some[x.body] :: BS] =
    new PXDecoder[FieldType[K, Option[H]] :: T, None.type :: AS, Some[x.body] :: BS] {
      private val empty = Option.empty[H].right[String]
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, Option[H]] :: T) =
        for {
          head <- in.x.body.cata(H.fromXml(_).map(Option(_)), empty)
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
    }

  implicit val cnil: CXDecoder[CNil, HNil, HNil] =
    new CXDecoder[CNil, HNil, HNil] {
      def from(in: XTag, as: HNil, bs: HNil): -\/[String] =
        fail("a valid typehint", in.asChild)
    }

  implicit def ccons[K <: Symbol, H, T <: Coproduct, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XDecoder[H]],
    T: CXDecoder[T, AS, BS]
  ): CXDecoder[FieldType[K, H] :+: T, None.type :: AS, None.type :: BS] =
    new CXDecoder[FieldType[K, H] :+: T, None.type :: AS, None.type :: BS] {
      val hint: XAttr = XAttr("typehint", XString(K.value.name))
      def from(
        in: XTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, H] :+: T) =
        if (in.attrs.element(hint))
          H.value.fromXml(in.asChild).map(a => Inl(field[K](a)))
        else
          T.from(in, as.tail, bs.tail).map(a => Inr(a))
    }

  implicit def cconsStr[
    K <: Symbol,
    H,
    T <: Coproduct,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: XStrDecoder[H],
    T: CXDecoder[T, AS, BS]
  ): CXDecoder[FieldType[K, H] :+: T, None.type :: AS, None.type :: BS] =
    new CXDecoder[FieldType[K, H] :+: T, None.type :: AS, None.type :: BS] {
      val hint: XAttr = XAttr("typehint", XString(K.value.name))
      def from(
        in: XTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, H] :+: T) =
        if (in.attrs.element(hint))
          in.body.cata(
            b => H.fromXml(b).map(a => Inl(field[K](a))),
            fail("a body", in.asChild)
          )
        else
          T.from(in, as.tail, bs.tail).map(a => Inr(a))
    }

  implicit def cconsStrTag[
    K <: Symbol,
    H,
    T <: Coproduct,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: XStrDecoder[H],
    T: CXDecoder[T, AS, BS]
  ): CXDecoder[FieldType[K, H] :+: T, None.type :: AS, Some[x.body] :: BS] =
    new CXDecoder[FieldType[K, H] :+: T, None.type :: AS, Some[x.body] :: BS] {
      private val key = K.value.name
      def from(
        in: XTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :+: T) =
        if (in.name == key)
          in.body.cata(
            b => H.fromXml(b).map(a => Inl(field[K](a))),
            fail("a body", in.asChild)
          )
        else
          T.from(in, as.tail, bs.tail).map(a => Inr(a))
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
    H: Lazy[XDecoder[H]],
    T: CXDecoder[T, AS, BS]
  ): CXDecoder[FieldType[K, H] :+: T, None.type :: AS, Some[x.body] :: BS] =
    new CXDecoder[FieldType[K, H] :+: T, None.type :: AS, Some[x.body] :: BS] {
      private val key = K.value.name
      def from(
        in: XTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :+: T) =
        if (in.name == key)
          H.value.fromXml(in.asChild).map(a => Inl(field[K](a)))
        else
          T.from(in, as.tail, bs.tail).map(a => Inr(a))
    }

  implicit def hcons[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XDecoder[H]],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] {
      private val key = K.value.name
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, H] :: T) = {
        val matching = XChildren(in.children.get(key))
        for {
          head <- H.value.fromXml(matching)
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
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
    H: Lazy[XDecoder[H]],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[
    FieldType[K, Option[H]] :: T,
    None.type :: AS,
    None.type :: BS
  ] =
    new PXDecoder[
      FieldType[K, Option[H]] :: T,
      None.type :: AS,
      None.type :: BS
    ] {
      private val key   = K.value.name
      private val empty = Option.empty[H].right[String]
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, Option[H]] :: T) = {
        val matching = in.children.get(key)
        for {
          head <- if (matching.isEmpty) empty
                 else H.value.fromXml(XChildren(matching)).map(Some(_))
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
      }
    }

  implicit def hconsStr[K <: Symbol, H, T <: HList, AS <: HList, BS <: HList](
    implicit
    K: Witness.Aux[K],
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, None.type :: BS] {
      private val key = K.value.name
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, H] :: T) = {
        val matching = in.children.get(key)
        for {
          head <- matching match {
                   case ICons(XTag(_, _, _, Maybe.Just(body)), INil()) =>
                     H.fromXml(body)
                   case _ => fail(s"one '$key' with a body", in.x.asChild)
                 }
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
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
    H: XStrDecoder[H],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[
    FieldType[K, Option[H]] :: T,
    None.type :: AS,
    None.type :: BS
  ] =
    new PXDecoder[
      FieldType[K, Option[H]] :: T,
      None.type :: AS,
      None.type :: BS
    ] {
      private val key   = K.value.name
      private val empty = Option.empty[H].right[String]
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: None.type :: BS
      ): String \/ (FieldType[K, Option[H]] :: T) = {
        val matching = in.children.get(key)
        for {
          head <- matching match {
                   case ICons(XTag(_, _, _, body), INil()) =>
                     body.cata(
                       b => H.fromXml(b).map(Option(_)),
                       empty
                     )
                   case INil() => empty
                   case _ =>
                     fail(s"one or none '$key' with a body", in.x.asChild)
                 }
          tail <- T.from(in, as.tail, bs.tail)
        } yield (field[K](head) :: tail)
      }
    }

  implicit def hconsInlinedMonoid[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    M: Monoid[H],
    H: Lazy[XDecoder[H]],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] {
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :: T) = {
        // ignores errors, failure to decode anything gives Monoid.empty
        val head = in.x.children
          .flatMap(ts => H.value.fromXml(ts.asChild).toMaybe.toIList)
          .fold
        for {
          tail <- T.from(in, as.tail, bs.tail)
        } yield field[K](head) :: tail
      }
    }
}

trait LowPriorityDerivedXDecoder1 extends LowPriorityDerivedXDecoder2 {
  this: DerivedXDecoder.type =>

  implicit def hconsInlinedSemigroup[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    M: Semigroup[H],
    H: Lazy[XDecoder[H]],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] {
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :: T) =
        T.from(in, as.tail, bs.tail).flatMap { tail =>
          val (fails, goods) =
            in.x.children.map(ts => H.value.fromXml(ts.asChild)).separate

          goods.toNel match {
            case Some(head) =>
              val folded = Foldable1[NonEmptyList].fold1(head)
              (field[K](folded) :: tail).right[String]
            case None =>
              val messages = fails.intercalate("\n")
              s"${K.value.name}:\n$messages".left
          }
        }
    }

}

trait LowPriorityDerivedXDecoder2 {
  this: DerivedXDecoder.type =>

  implicit def hconsInlined[
    K <: Symbol,
    H,
    T <: HList,
    AS <: HList,
    BS <: HList
  ](
    implicit
    K: Witness.Aux[K],
    H: Lazy[XDecoder[H]],
    T: PXDecoder[T, AS, BS]
  ): PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] =
    new PXDecoder[FieldType[K, H] :: T, None.type :: AS, Some[x.body] :: BS] {
      private val key = K.value.name
      def from(
        in: FastXTag,
        as: None.type :: AS,
        bs: Some[x.body] :: BS
      ): String \/ (FieldType[K, H] :: T) =
        T.from(in, as.tail, bs.tail).flatMap { tail =>
          val (fails, goods) =
            in.x.children.map(ts => H.value.fromXml(ts.asChild)).separate

          goods.into {
            case ICons(a, INil()) => (field[K](a) :: tail).right[String]
            case other if other.isEmpty =>
              val messages = fails.intercalate("\n")
              s"$key:\n$messages".left
            case _ =>
              fail(s"only one '${key}'", in.x.asChild)
          }
        }
    }

}
