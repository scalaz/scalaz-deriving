// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package cord

import java.util.regex.{ Matcher, Pattern }
import scalaz._, Scalaz._

object CordEncoder {
  import Cord.{ stringToCord => s2c }

  def encode(t: XTag): String = toCord(t).toString

  def toCord(t: XTag): Cord = preamble ++ xtag(t, 0)

  private[this] def fromList(cs: IList[Cord]): Cord       = cs.fold
  private[this] def fromNel(cs: NonEmptyList[Cord]): Cord = cs.fold

  private[this] val preamble: Cord = s2c(
    "<?xml version='1.0' encoding='UTF-8'?>"
  )
  private[this] val space: Cord = s2c(" ")
  private[this] val gt: Cord    = s2c(">")
  private[this] val egt: Cord   = s2c("/>")
  private[this] val lt: Cord    = s2c("<")
  private[this] val elt: Cord   = s2c("</")

  private[this] def xtag(t: XTag, level: Int): Cord = {
    val name = s2c(t.name)
    val start = {
      val open = Cord(pad(level), lt, name)
      if (t.attrs.isEmpty)
        open
      else {
        val attrs = t.attrs.map(xattr).intersperse(space)
        fromList(open :: space :: attrs)
      }
    }

    if (t.children.isEmpty && t.body.isEmpty)
      start ++ egt
    else
      t.children.toNel match {
        case None =>
          val body = t.body.map(xstring(_)).orZero
          val end  = Cord(elt, name, gt)
          Cord(start, gt, body, end)

        case Some(cs) =>
          val children = fromNel(cs.map(xtag(_, level + 1)))
          val body     = t.body.map(s => pad(level + 1) |+| xstring(s)).orZero
          val end      = Cord(elt, name, gt)
          Cord(start, gt, children, body, pad(level), end)
      }
  }

  private[this] val pad: Int => Cord = Memo.arrayMemo[Cord](16).apply(pad0(_))
  private[this] def pad0(level: Int): Cord =
    s2c("\n" + (" " * 2 * level))

  private[this] def xattr(a: XAttr): Cord =
    s2c(
      s"""${a.name}="${CordEncoder.replaceXmlEntities(a.value.text)}""""
    )

  private[this] def xstring(s: XString): Cord =
    if (!CordEncoder.containsXmlEntities(s.text))
      s2c(s.text)
    else {
      val matcher = CordEncoder.cdata.matcher(s.text)
      val clean =
        if (!matcher.find()) s.text
        else matcher.replaceAll(CordEncoder.nested)
      s2c(s"<![CDATA[$clean]]>")
    }

  private[cord] val cdata  = Pattern.compile("]]>", Pattern.LITERAL)
  private[cord] val nested = Matcher.quoteReplacement("]]]]><![CDATA[>")

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
