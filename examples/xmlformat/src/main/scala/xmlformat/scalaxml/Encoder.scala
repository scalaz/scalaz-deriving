// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import java.net.URLEncoder

import scalaz._
import simulacrum._

/**
 * Encodes the xmlformat ADT into the `scala.xml.NodeSeq` suite of classes.
 *
 * Note that the `NodeSeq` trait is unsealed and there are many anonymous
 * implementations.
 */
@typeclass
trait Encoder[A] { self =>
  def toScalaXml(a: A): xml.Node
}

object Encoder {
  implicit val contravariant: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] =
        b => fa.toScalaXml(f(b))
    }

  implicit val xnode: Encoder[XNode] = {
    case XString(text) =>
      val encoded = URLEncoder.encode(text, "UTF-8")
      if (encoded == text)
        xml.Unparsed(text) // avoids further xml.Text encoding checks
      else
        xml.PCData(text)
    case XChildren(ICons(XTag(name, attrs, children, body), INil())) =>
      // I heard you like sequences, so I made a linked list for you, inside
      // your NodeSeq, which is also a Seq[Node].
      val metadata = attrs.foldRight(xml.Null: xml.MetaData) {
        case (XAttr(name, XString(text)), meta) =>
          new xml.UnprefixedAttribute(
            name,
            xml.Text(text),
            meta
          )
      }

      val content =
        body.map(xnode.toScalaXml).toOption.toList :::
          children.map(t => xnode.toScalaXml(XChildren(IList(t)))).toList

      xml.Elem(
        null, // scalafix:ok
        name,
        metadata,
        xml.TopScope,
        true,
        content.toSeq: _*
      )

    case XChildren(list) =>
      xml.Group(list.map(t => xnode.toScalaXml(XChildren(IList(t)))).toList)
  }
}
