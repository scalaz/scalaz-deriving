// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import javax.xml.parsers.{ SAXParser, SAXParserFactory }

import scalaz._, Scalaz._
import simulacrum._

@typeclass(generateAllOps = false)
trait Decoder[A] { self =>
  def fromScalaXml(x: xml.NodeSeq): String \/ A
}

object Decoder {
  @inline def instance[A](f: xml.NodeSeq => String \/ A): Decoder[A] = f(_)

  import Isomorphism.<~>
  val iso: Decoder <~> Kleisli[String \/ ?, xml.NodeSeq, ?] = Kleisli.iso(
    λ[λ[a => (xml.NodeSeq => String \/ a)] ~> Decoder](instance(_)),
    λ[Decoder ~> λ[a => (xml.NodeSeq => String \/ a)]](_.fromScalaXml)
  )
  implicit val monad: MonadError[Decoder, String] = MonadError.fromIso(iso)

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
    (secureLoadString(txt) >>= xnode.fromScalaXml).flatMap {
      case XString(_)        => -\/("failed to parse raw string data")
      case ts @ XChildren(_) => \/-(ts)
    }

  implicit val xnode: Decoder[XNode] = {
    case _: xml.Comment | _: xml.ProcInstr | _: xml.EntityRef =>
      // things we ignore
      \/-(XChildren(IList.empty))
    case d: xml.Document =>
      (Option(d.docElem) \/> "no content") >>= xnode.fromScalaXml

    case t: xml.Text => \/-(XText(t.data))
    case p: xml.PCData =>
      \/-(XCdata(p.data.replaceAll("""]]]]><!\[CDATA\[>""", "]]>")))
    case u: xml.Unparsed => -\/(s"encountered unparsed xml: ${u.data}")

    case xml.Group(nodes) =>
      nodes.toList.toIList
        .traverse(xnode.fromScalaXml)
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
        .fromScalaXml(
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
