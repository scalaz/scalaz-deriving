// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{ -\/, @@, \/, \/-, ICons, IList, INil }
import scalaz.Scalaz._

import shapeless._
import shapeless.labelled._

/**
 * Product and coproduct encoder.
 *
 * We get into problems with ambiguous decoding if we do not encode products to
 * a single tag. Therefore all return values are single entry XChildren.
 *
 * = Fields =
 *
 * For the following
 *
 * {{{
 * case class Bar(wobble: String)
 * case class Foo(wibble: Bar)
 * }}}
 *
 * the default encoding is
 *
 * {{{
 * <Bar><wobble>...</wobble></Bar>
 * <Foo><wibble><wobble>...</wobble></wibble></Foo>
 * }}}
 *
 * where each case class gets assigned an XTag according to its name, which is
 * overwritten when included as part of an outer tag. Attributes and body are
 * preserved.
 *
 * An alternative encoding is available using the Inlined which will use the
 * provided encoding directly, ignoring the field name, e.g.
 *
 * {{{
 * case class Baz(wibble: Bar @@ XInlined)
 * <Baz><Bar><wobble>...</wobble></Bar></Baz>
 * }}}
 *
 * this is useful when the decoder may use the tag name to distinguish between
 * coproducts.
 *
 * {{{
 * sealed trait AA
 * case object B extends AA
 * case object C extends AA
 *
 * case class A(as: List[AA] @@ XInlined)
 *
 * <A>
 *   <B></B>
 *   <B></B>
 *   <C></C>
 *   <T/>
 * </A>
 * }}}
 *
 * If the XInlined type has a Monoid, failure to decode anything will give Monoid.empty.
 * If it has a Semigroup, one or more values must be decoded or there will be a failure.
 * If there is neither, exactly one value is expected.
 *
 * = Coproducts =
 *
 * Coproducts are differentiated by a "typehint" attribute. Recall that tag
 * names can be rewritten by a container, therfore it is not feasible in the
 * general case to use tag names to differentiate between coproduct values.
 *
 * However, if a coproduct has derived an XEncoderTag instead of an XEncoder,
 * tags will be used to differentiate instead of attributes. If used as a field
 * in a container, it **must** be XInlined, which is enforced by the types.
 *
 * = Attributes =
 *
 * {{{
 * case class Fuh(foo: String @@ XAttribute)
 *
 * <Fuh foo="Hello world"/>
 * }}}
 *
 * Attributes can be specified with the `@@ XAttribute` tag and can only be used
 * on types that have an XStrEncoder. Option fields are handled specially and
 * a None value will not appear in the output.
 *
 * = String content =
 *
 * {{{
 * case class Duh(body: String @@ XInlined)
 *
 * <Duh>Hello world!</Duh>
 * }}}
 *
 * Values can be specfied as the content of the tag with the `@@ XInlined`
 * if they have an XStrEncoder. It is the developer's responsibility to ensure
 * that only one field has this tag. Multiple content fields will be
 * concatenated and may not be decoded.
 */
sealed trait DerivedXEncoder[R] {
  // a union type would be very nice here...
  private[generic] def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag
}
object DerivedXEncoder extends LowPriorityDerivedXEncoder1 {

  def gen[T, Repr](
    implicit
    T: Typeable[T],
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedXEncoder[Repr]]]
  ): XEncoder[T] = { t =>
    LER.value.value.to(G.to(t)) match {
      case -\/(product) =>
        val (attrs, content) = product.separate
        val (children, body) = content.separate
        XTag(T.describe, attrs, children, body.fold1Opt.toMaybe).asChild
      case \/-(coproduct) => coproduct.copy(name = T.describe).asChild
    }
  }

  trait PXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag = -\/(product(r))
    def product(r: R): IList[XAttr \/ (XTag \/ XString)]
  }
  trait CXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag = \/-(coproduct(r))
    def coproduct(r: R): XTag
  }

  implicit val hnil: PXEncoder[HNil] = _ =>
    IList.empty[XAttr \/ (XTag \/ XString)]

  implicit def hconsAttr[K <: Symbol, A, T <: HList](
    implicit
    K: Witness.Aux[K],
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A @@ XAttribute] :: T] = {
    case head :: tail =>
      val key   = K.value.name
      val value = EV.toXml(XAttribute.unwrap(head))
      -\/(XAttr(key, value)) :: ER.product(tail)
  }

  implicit def hconsAttrOptional[K <: Symbol, A, T <: HList](
    implicit
    K: Witness.Aux[K],
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, Option[A] @@ XAttribute] :: T] = {
    case head :: tail =>
      XAttribute.unwrap(head) match {
        case None => ER.product(tail)
        case Some(h) =>
          hconsAttr(K, EV, ER).product(field[K](XAttribute(h)) :: tail)
      }
  }
  implicit def hconsInlinedField[K <: Symbol, A, T <: HList](
    implicit
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A @@ XInlined] :: T] = {
    case head :: tail =>
      LEV.value
        .toXml(XInlined.unwrap(head))
        .tree
        .map(_.left[XString].right[XAttr]) ::: ER.product(tail)
  }

  // tags only allowed inline
  implicit def hconsInlinedTag[K <: Symbol, A, T <: HList](
    implicit
    LEV: Lazy[XEncoderTag[A]],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A @@ XInlined] :: T] = {
    case head :: tail =>
      LEV.value
        .toXTag(XInlined.unwrap(head))
        .left[XString]
        .right[XAttr] :: ER.product(tail)
  }

  implicit def hconsInlinedStr[K <: Symbol, A, T <: HList](
    implicit
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A @@ XInlined] :: T] = {
    case head :: tail =>
      \/-(\/-(EV.toXml(XInlined.unwrap(head)))) :: ER.product(tail)
  }

  implicit def hconsInlinedStrOptional[K <: Symbol, A, T <: HList](
    implicit
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, Option[A] @@ XInlined] :: T] = {
    case head :: tail =>
      XInlined.unwrap(head) match {
        case None => ER.product(tail)
        case Some(h) =>
          hconsInlinedStr(EV, ER).product(
            field[K](XInlined(h)) :: tail
          )
      }
  }

  private[this] val typehint: String = "typehint"
  implicit val cnil: CXEncoder[CNil] = _ => sys.error("bad coproduct")
  implicit def ccons[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LEI: Lazy[XNodeEncoder[A]],
    ER: CXEncoder[T]
  ): CXEncoder[FieldType[K, A] :+: T] = {
    case Inl(ins) =>
      val hint = XAttr(typehint, XString(K.value.name))
      LEI.value.toXml(ins) match {
        case XChildren(ICons(XTag(n, as, ts, b), INil())) =>
          XTag(n, hint :: as, ts, b)
        case c =>
          XTag("value", c).copy(attrs = IList.single(hint))
      }

    case Inr(rem) => ER.coproduct(rem)
  }

}

trait LowPriorityDerivedXEncoder1 extends LowPriorityDerivedXEncoder2 {
  this: DerivedXEncoder.type =>

  implicit def hcons[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A] :: T] = {
    case head :: tail =>
      LEV.value.toXml(head).tree.map {
        case XTag(_, as, ts, bd) =>
          XTag(K.value.name, as, ts, bd).left[XString].right[XAttr]
      } ::: ER.product(tail)
  }

  implicit def hconsOptional[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, Option[A]] :: T] = {
    case head :: tail =>
      (head: Option[A]) match {
        case None => ER.product(tail)
        case Some(value) =>
          hcons(K, LEV, ER).product(field[K](value) :: tail)
      }
  }

  implicit def hconsStr[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A] :: T] = {
    case head :: tail =>
      val tag = XTag(K.value.name, EV.toXml(head))
      \/-(-\/(tag)) :: ER.product(tail)
  }

  implicit def hconsStrOptional[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    EV: XStrEncoder[A],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, Option[A]] :: T] = {
    case head :: tail =>
      (head: Option[A]) match {
        case None => ER.product(tail)
        case Some(value) =>
          hconsStr(K, EV, ER).product(field[K](value) :: tail)
      }
  }

}

trait LowPriorityDerivedXEncoder2 {
  this: DerivedXEncoder.type =>

  // WORKAROUND https://github.com/milessabin/shapeless/issues/309
  implicit def hconsHack[K <: Symbol, A, Z, T <: HList](
    implicit K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A @@ Z]],
    ER: PXEncoder[T]
  ): PXEncoder[FieldType[K, A @@ Z] :: T] = hcons(K, LEV, ER)
}
