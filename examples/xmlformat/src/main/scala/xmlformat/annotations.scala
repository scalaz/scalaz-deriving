// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.x

import scala.annotation.Annotation

/**
 * When annotating a product field, uses the `XStrEncoder` / `XStrDecoder` to
 * encode / decode an attribute on the enclosing element.
 */
final class attr extends Annotation

/**
 * When annotating a product field that has an `XStrEncoder` / `XStrDecoder`,
 * will include the content as the body of the enclosing element.
 *
 * When annotating a product field that has an `XEncoder` / `XDecoder`, will
 * include the children directly into the body of the enclosing element without
 * adding field names. When decoding, concatenation of multiple elements depends
 * on whether the type has a `Semigroup`, `Monoid` or neither.
 *
 * When annotating a `case class` as a member of a coproduct, that has an
 * `XEncoder` / `XDecoder`, uses the typename of the class to disambiguate
 * rather than using the `"typehint"` attribute. `XStrEncoder` / `XStrDecoder`
 * members of coproducts get wrapped in their type's name.
 */
final class body extends Annotation
