// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

import scalaz._

package xmlformat {

  /** A product field is encoded as an XAttr, not an XTag. */
  sealed trait XAttribute

  /**
   * A product field uses the tag name provided by the nested encoder instead of
   * the container.
   *
   * Note that there is a performance and error reporting penalty to decoding an
   * inlined field. The decoder must attempt to decode every field and will
   * throw away error messages, since there is no way to know if an error is
   * expected or unexpected.
   *
   * This behaves differently depending on
   *
   * - if the field has a Str encoder / decoder
   * - if the field type has a Monoid (failure to decode gives empty)
   * - if the field type has a Semigroup (must decode one or more values)
   * - if the field type has no Monoid / Semigroup (must decode exactly one)
   *
   * For a detailed description, see DerivedXEncoder.
   */
  sealed trait XInlined

  /** Automatic conversions for our specific tagged types */
  object implicits {
    // scalafix:off DisableSyntax.implicitConversion
    @inline implicit def toXAttr[A](a: A): A @@ XAttribute = XAttribute(a)
    @inline implicit def fromXAttr[A](a: A @@ XAttribute): A =
      XAttribute.unwrap(a)
    @inline implicit def toXInlined[A](a: A): A @@ XInlined = XInlined(a)
    @inline implicit def fromXInlined[A](a: A @@ XInlined): A =
      XInlined.unwrap(a)
    // scalafix:on DisableSyntax.implicitConversion
  }
}

package object xmlformat {
  val XAttribute: Tag.TagOf[XAttribute] = Tag.of[XAttribute]
  val XInlined: Tag.TagOf[XInlined]     = Tag.of[XInlined]
}
