// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import javax.xml.parsers.SAXParserFactory

import scalaz._, Scalaz._

@simulacrum.typeclass(generateAllOps = false)
trait Decoder[A] { self =>
  def fromXml(x: xml.NodeSeq): Decoder.Decoded[A]

  def map[B](f: A => B): Decoder[B] =
    xml => self.fromXml(xml).map(f)
  def andThen[B](f: A => Decoder.Decoded[B]): Decoder[B] =
    xml => self.fromXml(xml).flatMap(f)
  def xmap[B](f: A => B, @unused g: B => A): Decoder[B] =
    map(f)

}

// DESNOTE(2017-12-22, SHalliday) an earlier version of this typeclass provided
// instances for a wide range of data types, but the ambiguity around the
// scala.xml object hierarchy was problematic. Hence, only the xmlformat.XNode
// ADT is supported.
object Decoder {

  /** Avoid security exploits, see https://github.com/scala/scala-xml/issues/17 */
  def secureParser() = {
    val f = SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    f.setFeature(
      "http://xml.org/sax/features/external-parameter-entities",
      false
    )
    f.setFeature("http://xml.org/sax/features/external-general-entities", false)
    f.setXIncludeAware(false)
    f.setNamespaceAware(false)
    f.newSAXParser()
  }
  def secureLoadString(txt: String) =
    xml.XML.loadXML(xml.Source.fromString(txt), secureParser())

  type Decoded[A] = String \/ A

  /** Convenient decoder to domain objects, via XNode */
  object ops extends ToDecoderOps {
    implicit class DecoderOps(private val t: xml.NodeSeq) extends AnyVal {
      def decode[A: XDecoder] =
        for {
          xnode <- Decoder[XNode].fromXml(t)
          a     <- XDecoder[A].fromXml(xnode)
        } yield a
    }
  }

  implicit val xnode: Decoder[XNode] = {
    case _: xml.Comment | _: xml.ProcInstr | _: xml.EntityRef =>
      // things we ignore
      \/-(XChildren(IList.empty))
    case d: xml.Document =>
      (Option(d.docElem) \/> "no content") >>= xnode.fromXml

    case t: xml.Text => \/-(XText(t.data))
    case p: xml.PCData =>
      \/-(XCdata(p.data.replaceAll("""]]]]><!\[CDATA\[>""", "]]>")))
    case u: xml.Unparsed => -\/(s"encountered unparsed xml: ${u.data}")

    case xml.Group(nodes) =>
      nodes.toList.toIList
        .traverse(xnode.fromXml)
        .map {
          case INil()                     => XChildren(IList.empty)
          case ICons(xs: XString, INil()) => xs
          case ICons(xt: XTag, INil())    => xt
          case other =>
            XChildren(other.flatMap {
              case xt: XTag => IList.single(xt)
              case _        => IList.empty // lossy
            })
        }

    case e: xml.Elem =>
      xnode
        .fromXml(
          e.child match {
            case Seq(only) => only
            case many      => xml.Group(many)
          }
        )
        .map {
          case xs: XString   => xs
          case xc: XChildren => xc
          case xt: XTag      => XChildren(IList(xt))
        }
        .map { content =>
          val name = XAtom(e.label)
          val attrs = e.attributes.asAttrMap.toList.toIList.map {
            case (k, v) => XAttr(XAtom(k), XText(v))
          }

          XTag(name, attrs, content)
        }

    case other => -\/(s"got unexpected ${other.getClass}: $other")
  }

}
