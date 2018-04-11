// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

import scalaz._

package xmlformat {

  /** A product field is encoded as an XAttr, not an XTag. */
  sealed trait XAttribute

  /** A list-like structure does not have a wrapper XTag. */
  sealed trait XInlinedList

  /**
   * A product field that is an XTag is used as both the key and value.
   *
   * Can be added to an XInlinedList, supporting duplicate keys.
   *
   * Consider if an intermediate `case class` suits your purpose. This is
   * typically only needed if the element has attributes or additional fields.
   */
  sealed trait XInlinedField

  /**
   * A product field that is not an XTag is encoded as the content of an
   * element.
   *
   * Consider if an intermediate `case class` suits your purpose. This is
   * typically only needed if the element has attributes.
   */
  sealed trait XInlinedContent

  // It is unfortunate that we need to have multiple Tags for inlining. However,
  // this is a deep consequence of the XML format and our design choices.
  // Consider the alternative where we have one tag. There are two ways to do
  // this:
  //
  // 1. make the default encoding for lists to be inlined. Although this works,
  //    it means we get into situations where we cannot roundtrip a list of
  //    anything that is not encoded as an XTag. So we can:
  //
  //    a) allow encoding to fail, which means no compiletime guarantees and we
  //       must deal with the output monadically.
  //    b) or, catch it at compiletime by differentiating between XNodeEncoder and
  //       XTagEncoder. But this would require a complete redesign, even of the
  //       ADT, since products can encode to XChildren.
  //
  //    with a similar digression for XInlinedContent.
  //
  // 2. or, we add a lot more shapeless / implicit magic to the deriver such
  //    that XInlined things are special cased if they have an implicit Foldable
  //    or CanBuildFrom. The implementation complexity goes through the roof,
  //    but this may be easier for users of the API, unless they are confused by
  //    the special casing of list-like structures, and super-special casing of
  //    inlined list-like structures that don't decode to XTag, which no longer
  //    roundtrip.

  /** Automatic conversions for our specific tagged types */
  object implicits {
    // scalafix:off
    @inline implicit def toXAttr[A](a: A): A @@ XAttribute = XAttribute(a)
    @inline implicit def fromXAttr[A](a: A @@ XAttribute): A =
      XAttribute.unwrap(a)
    @inline implicit def toXInlinedList[A](a: A): A @@ XInlinedList =
      XInlinedList(a)
    @inline implicit def fromXInlinedList[A](a: A @@ XInlinedList): A =
      XInlinedList.unwrap(a)
    @inline implicit def toXInlinedField[A](a: A): A @@ XInlinedField =
      XInlinedField(a)
    @inline implicit def fromXInlinedField[A](a: A @@ XInlinedField): A =
      XInlinedField.unwrap(a)

    @inline implicit def toXInlinedListField[A](
      a: A
    ): A @@ XInlinedList @@ XInlinedField =
      XInlinedField(XInlinedList(a))
    @inline implicit def fromXInlinedListField[A](
      a: A @@ XInlinedList @@ XInlinedField
    ): A =
      XInlinedList.unwrap(XInlinedField.unwrap(a))

    @inline implicit def toXInlinedContent[A](a: A): A @@ XInlinedContent =
      XInlinedContent(a)
    @inline implicit def fromXInlinedContent[A](a: A @@ XInlinedContent): A =
      XInlinedContent.unwrap(a)
    // scalafix:on
  }

}

package object xmlformat {
  val XAttribute: Tag.TagOf[XAttribute] = Tag.of[XAttribute]

  val XInlinedList: Tag.TagOf[XInlinedList] = Tag.of[XInlinedList]

  val XInlinedField: Tag.TagOf[XInlinedField] = Tag.of[XInlinedField]

  val XInlinedContent: Tag.TagOf[XInlinedContent] = Tag.of[XInlinedContent]
}
