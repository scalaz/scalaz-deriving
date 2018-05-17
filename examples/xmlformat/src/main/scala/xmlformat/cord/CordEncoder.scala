// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package cord

import java.util.regex.{ Matcher, Pattern }
import scalaz._, Scalaz._

object CordEncoder {
  def encode(t: XTag): String = toCord(t).toString

  def toCord(t: XTag): Cord = preamble |+| xtag(t, 0)

  private[this] val preamble: Cord = Cord.stringToCord(
    "<?xml version='1.0' encoding='UTF-8'?>"
  )
  private[this] val space: Cord = Cord.stringToCord(" ")
  private[this] val gt: Cord    = Cord.stringToCord(">")
  private[this] val egt: Cord   = Cord.stringToCord("/>")
  private[this] val lt: Cord    = Cord.stringToCord("<")
  private[this] val elt: Cord   = Cord.stringToCord("</")

  private[this] def xtag(t: XTag, level: Int): Cord = {
    val name = Cord.stringToCord(t.name)
    val start = {
      val open = pad(level) ++ lt ++ name
      if (t.attrs.isEmpty)
        open
      else {
        val attrs = t.attrs.map(xattr).intersperse(space).fold
        open ++ space ++ attrs
      }
    }

    if (t.children.isEmpty && t.body.isEmpty)
      start ++ egt
    else {
      val children = t.children.foldMap(xtag(_, level + 1))
      val padding  = if (t.children.isEmpty) Cord.empty else pad(level)
      def spacing  = if (t.children.isEmpty) Cord.empty else pad(level + 1)
      val body     = t.body.map(s => spacing ++ xstring(s)).orZero
      val end      = elt ++ name ++ gt
      start ++ gt ++ children ++ body ++ padding ++ end
    }
  }

  private[this] val pad: Int => Cord       = Memo.arrayMemo[Cord](16).apply(pad0(_))
  private[this] def pad0(level: Int): Cord = "\n" + (" " * 2 * level)

  private[this] def xattr(a: XAttr): Cord =
    s"""${a.name}="${replaceXmlEntities(a.value.text)}""""

  private[this] def xstring(s: XString): Cord =
    if (!containsXmlEntities(s.text))
      s.text
    else {
      val matcher = cdata.matcher(s.text)
      val clean =
        if (!matcher.find()) s.text
        else matcher.replaceAll(nested)
      s"<![CDATA[$clean]]>"
    }

  private[this] val cdata  = Pattern.compile("]]>", Pattern.LITERAL)
  private[this] val nested = Matcher.quoteReplacement("]]]]><![CDATA[>")

  // https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references#Predefined_entities_in_XML
  def replaceXmlEntities(input: String): String = {
    // scalafix:off DisableSyntax.keywords.while
    val m = entities.matcher(input)
    if (!m.find()) input
    else {
      val sb = new StringBuffer
      do {
        val repl = m.group(0) match {
          case "&"  => "&amp;"
          case "\"" => "&quot;"
          case "'"  => "&apos;"
          case "<"  => "&lt;"
          case ">"  => "&gt;"
        }
        m.appendReplacement(sb, repl)
      } while (m.find())
      m.appendTail(sb)
      sb.toString
    }
    // scalafix:on DisableSyntax.keywords.while
  }

  private[this] val entities = Pattern.compile("""("|&|'|<|>)""")

  def containsXmlEntities(input: String): Boolean =
    entities.matcher(input).find()

}
