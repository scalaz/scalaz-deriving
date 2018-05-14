// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

/**
 * ADT for XML, unaware of schemas, namespaces and comments (which are all
 * needlessly complicated and unnecessary for marshalling).
 */
sealed abstract class XNode

/**
 * An xml tag, also known as an element.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-NameChar (not enforced).
 *
 * This is not an XNode, otherwise there is ambiguity with a one element list.
 */
final case class XTag(name: String,
                      attrs: IList[XAttr],
                      children: IList[XTag],
                      body: Maybe[XString]) {
  def asChild: XChildren = XChildren(IList(this))
}
object XTag {
  def apply(key: String, content: XNode): XTag = content match {
    case XChildren(c)   => XTag(key, IList.empty, c, Maybe.empty)
    case s @ XString(_) => XTag(key, IList.empty, IList.empty, Maybe.just(s))
  }
}

/**
 * An attribute: key and value.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-AttValue (not enforced).
 */
final case class XAttr(name: String, value: XString)
object XAttr {
  implicit val equal: Equal[XAttr] =
    Divide[Equal].deriving2(a => (a.name, a.value))
}

/**
 * A raw variant of children in an XTag.
 */
final case class XChildren(tree: IList[XTag]) extends XNode

/** String content for an XAttr value or XTag body. */
@xderiving(Semigroup, Equal)
final case class XString(text: String) extends XNode
