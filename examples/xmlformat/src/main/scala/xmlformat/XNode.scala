// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._

/**
 * ADT for XML, unaware of schemas, namespaces and comments (which are all
 * needlessly complicated and unnecessary for marshalling).
 *
 * May have backends for
 *
 * - https://github.com/scala/scala-xml
 * - Java DOM
 * - https://github.com/propensive/xylophone
 *
 * Compare to the (fuller) Haskell ADTs
 *
 * - http://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-DOM-TypeDefs.html#t:XNode
 * - https://hackage.haskell.org/package/xml-conduit-1.7.0/docs/Text-XML.html
 *
 * This is somewhere in between.
 */
sealed abstract class XNode {
  def asContent: XContent = this match {
    case t: XTag     => XChildren(IList.single(t))
    case x: XContent => x
  }
}

/**
 * An xml tag, also known as an element.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-NameChar (not enforced).
 */
final case class XTag(name: XAtom, attrs: IList[XAttr], children: XContent)
    extends XNode

/**
 * An attribute: key and value.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-AttValue (not enforced).
 */
final case class XAttr(name: XAtom, value: XString)

/** May be the body of an XTag */
sealed abstract class XContent                extends XNode
final case class XChildren(tree: IList[XTag]) extends XContent

/**
 * When decoding, string data may arrive in a variety of forms, this gives us
 * the ability to treat all equally.
 */
sealed abstract class XString extends XContent
object XString {
  def unapply(s: XString): Option[String] = s match {
    case XText(text)  => Some(text)
    case XCdata(text) => Some(text)
    case XAtom(text)  => Some(text)
    case _            => None
  }
}

/** Text that may require URL encoding for transport. */
final case class XText(text: String) extends XString

/** Text that may require CDATA encoding for transport. */
final case class XCdata(text: String) extends XString

/** Text that does not require any escaping. */
final case class XAtom(text: String) extends XString
