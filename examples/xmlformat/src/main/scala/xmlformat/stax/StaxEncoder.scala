// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package stax

import java.io.StringWriter
import java.util.regex.Pattern

import javax.xml.stream.{ XMLOutputFactory, XMLStreamWriter }
import scalaz._, Scalaz._

import com.ctc.wstx.stax.WstxOutputFactory

object StaxEncoder {
  // must not escape the code in this module
  private[this] val factory = new ThreadLocal[XMLOutputFactory] {
    override def initialValue: WstxOutputFactory = {
      val f = new com.ctc.wstx.stax.WstxOutputFactory
      f.configureForSpeed()
      f.getConfig.doSupportNamespaces(false)
      f
    }
  }

  def encode(t: XTag): String = {
    val output = new StringWriter

    val x = factory.get.createXMLStreamWriter(output)
    x.writeStartDocument()
    writeTag(x, t, 0)
    x.writeEndDocument()
    output.toString()
  }

  private[this] def writeTag(x: XMLStreamWriter, t: XTag, level: Int): Unit = {
    x.writeCharacters("\n")
    x.writeCharacters(" " * 2 * level)
    x.writeStartElement(t.name)

    t.attrs.toList.foreach { a =>
      x.writeAttribute(a.name, a.value.text)
    }

    t.children.toList.foreach { c =>
      writeTag(x, c, level + 1)
    }

    t.body.toList.foreach { s =>
      if (t.children.nonEmpty) {
        x.writeCharacters("\n")
        x.writeCharacters(" " * 2 * (level + 1))
      }
      if (!containsXmlEntities(s.text))
        x.writeCharacters(s.text)
      else {
        val clean =
          if (!s.text.contains("]]>")) s.text
          else s.text.replace("]]>", "]]]]><![CDATA[>")
        x.writeCData(clean)
      }
    }

    if (t.children.nonEmpty) {
      x.writeCharacters("\n")
      x.writeCharacters(" " * 2 * level)
    }

    x.writeEndElement()
  }

  private[this] val entities                      = Pattern.compile("""("|&|'|<|>)""")
  def containsXmlEntities(input: String): Boolean =
    entities.matcher(input).find()

}
