// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.annotation.switch
import scalaz._, Scalaz._
import internal.TCord

object PrettyPrinter {
  def apply(j: JsValue): String = print(j, 0).shows

  private def print(j: JsValue, level: Int): TCord =
    j match {
      case JsArray(as)  =>
        "[" :: as.map(print(_, level)).intercalate(", ") ++ "]"
      case JsObject(fs) =>
        "{" :: (fs.map(print(_, level + 1)).intercalate(",") ++ pad(
          level
        )) ++ "}"
      case _            => CompactPrinter.print(j)
    }

  private def print(entry: (String, JsValue), level: Int): TCord =
    (pad(level) ++ CompactPrinter.escaped(entry._1) ++ ": ") ++ print(
      entry._2,
      level
    )

  private[this] val pad: Int => TCord       = Memo.arrayMemo[TCord](16).apply(pad0(_))
  private[this] def pad0(level: Int): TCord =
    "\n" + (" " * 2 * level)

}

object CompactPrinter {
  def apply(j: JsValue, cb: String): String =
    (cb :: ("(" :: print(j) ++ ")")).shows

  def apply(j: JsValue): String = print(j).shows

  private[jsonformat] def print(j: JsValue): TCord =
    j match {
      case JsNull       => "null"
      case JsBoolean(v) => if (v) "true" else "false"
      case JsDouble(n)  => n.toString
      case JsInteger(n) => n.toString
      case JsString(s)  => escaped(s)
      case JsArray(as)  => "[" :: as.map(print).intercalate(",") ++ "]"
      case JsObject(fs) => "{" :: fs.map(print).intercalate(",") ++ "}"
    }

  private def print(entry: (String, JsValue)): TCord =
    escaped(entry._1) :: ":" :: print(entry._2)

  private[jsonformat] def escaped(s: String): String = {
    // scalafix:off
    val sb = new java.lang.StringBuilder
    var i  = 0
    sb.append("\"")
    while (i < s.length) {
      sb.append(escape(s(i)))
      i += 1
    }
    sb.append("\"")
    sb.toString
    // scalafix:on
  }

  // https://www.ietf.org/rfc/rfc4627.txt
  private def escape(c: Char): String =
    (c: @switch) match {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    =>
        if (Character.isISOControl(c)) "\\u%04x".format(c.toInt) else c.toString
    }

}
