// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import javax.xml.parsers.{ SAXParser, SAXParserFactory }

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

object Decoder {

  /** Avoid security exploits, see https://github.com/scala/scala-xml/issues/17 */
  private[this] def secureParser(): SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    f.setFeature(
      "http://xml.org/sax/features/external-parameter-entities",
      false
    )
    f.setFeature("http://xml.org/sax/features/external-general-entities", false)
    f.setXIncludeAware(false)
    f.setNamespaceAware(false)
    f.newSAXParser()
  }

  // must not escape the code in this module
  private[this] val cachedParser = new ThreadLocal[SAXParser] {
    override def initialValue: SAXParser = secureParser()
  }

  def secureLoadString(txt: String): String \/ xml.Elem =
    \/.attempt(xml.XML.loadXML(xml.Source.fromString(txt), cachedParser.get())) {
      case t: Throwable => s"failed to parse as xml, message: ${t.getMessage}"
    }

  def parse(txt: String): String \/ XChildren =
    (secureLoadString(txt) >>= xnode.fromXml).flatMap {
      case XString(_)        => -\/("failed to parse raw string data")
      case ts @ XChildren(_) => \/-(ts)
    }

  type Decoded[A] = String \/ A

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
          case ICons(xs: XString, INil()) => xs
          case other =>
            val tags = other.flatMap {
              case XChildren(tags) => tags
              case _               => IList.empty[XTag] // lossy
            }
            XChildren(tags)
        }

    case e: xml.Elem =>
      xnode
        .fromXml(
          e.child match {
            case Seq(only) => only
            case many      => xml.Group(many)
          }
        )
        .map { content =>
          val name = XAtom(e.label)
          val attrs = e.attributes.asAttrMap.toList.toIList.map {
            case (k, v) => XAttr(XAtom(k), XText(v))
          }

          XTag(name, content).copy(attrs = attrs).asChild
        }

    case other => -\/(s"got unexpected ${other.getClass}: $other")
  }

}
