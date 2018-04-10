// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import scalaz.unused

/**
 * Encodes the xmlformat ADT into the `scala.xml.NodeSeq` suite of classes.
 *
 * Note that the `NodeSeq` trait is unsealed and there are many anonymous
 * implementations.
 */
@simulacrum.typeclass
trait Encoder[A] { self =>
  def toScalaXml(a: A): xml.Node

  def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def toScalaXml(b: B): xml.Node =
      self.toScalaXml(f(b))
  }
  def xmap[B](@unused f: A => B, g: B => A): Encoder[B] =
    contramap(g)
}

object Encoder {

  implicit val xnode: Encoder[XNode] = {
    case XText(text)                              => xml.Text(text)
    case XCdata(text)                             => xml.PCData(text)
    case XAtom(text)                              => xml.Unparsed(text)
    case XTag(XAtom(name), attrs, children, body) =>
      // I heard you like sequences, so I made a linked list for you, inside
      // your NodeSeq, which is also a Seq[Node].
      val metadata = attrs.foldRight(xml.Null: xml.MetaData) {
        case (XAttr(name, XString(text)), meta) =>
          new xml.UnprefixedAttribute(
            name.text,
            xml.Text(text),
            meta
          )
      }

      val content =
        body.map(xnode.toScalaXml).toOption.toList :::
          children.map(xnode.toScalaXml).toList

      xml.Elem(
        null,
        name,
        metadata,
        xml.TopScope,
        true,
        content.toSeq: _*
      )

    case XChildren(list) =>
      xml.Group(list.map(xnode.toScalaXml).toList)
  }
}
