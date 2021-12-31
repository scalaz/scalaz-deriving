// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package stax

import java.io.StringReader
import javax.xml.stream.{ XMLInputFactory, XMLStreamConstants, XMLStreamReader }
import scalaz._, Scalaz._
import com.ctc.wstx.stax.WstxInputFactory

// scalafix:off DisableSyntax.keywords.while,DisableSyntax.keywords.var
object StaxDecoder {
  // must not escape the code in this module
  private[this] val factory = new ThreadLocal[XMLInputFactory] {
    override def initialValue: WstxInputFactory = {
      val f = new com.ctc.wstx.stax.WstxInputFactory
      f.configureForSpeed()
      f
    }
  }

  import XMLStreamConstants._
  def parse(txt: String): String \/ XTag = {
    val reader = factory.get.createXMLStreamReader(new StringReader(txt))
    try {
      reader.nextTag()
      parseTag(reader).right
    } catch {
      case e: Exception =>
        s"parser error: ${e.getMessage} ${e.getClass}".left
    } finally reader.close()
  }

  private[this] def parseTag(x: XMLStreamReader): XTag = {
    val name  = x.getName.getLocalPart()
    val attrs = 0.until(x.getAttributeCount).toList.map { i =>
      XAttr(
        x.getAttributeLocalName(i),
        XString(x.getAttributeValue(i))
      )
    }

    var children = IList.empty[XTag]
    var content  = IList.empty[String]

    x.next()
    while (x.getEventType() != END_ELEMENT) {
      x.getEventType() match {
        case START_ELEMENT      =>
          children = parseTag(x) :: children
        case CHARACTERS | CDATA =>
          val text = x.getText().trim
          if (!text.isEmpty)
            content = text :: content
        case _                  =>
      }
      x.next()
    }

    val body = content.toNel.map(t => XString(t.reverse.fold)).toMaybe

    XTag(name, attrs.toIList, children.reverse, body)
  }
}
