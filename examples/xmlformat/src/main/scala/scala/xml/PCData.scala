// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright: 2003-2013, LAMP/EPFL
// License: https://github.com/scala/scala/blob/2.13.x/LICENSE

// backport https://github.com/scala/scala-xml/pull/171
package scala
package xml

import java.lang.String

import scala.Predef.augmentString

class PCData(data: String) extends Atom[String](data) {
  override def buildString(sb: StringBuilder): StringBuilder =
    sb append "<![CDATA[%s]]>".format(data)
}
object PCData {
  def apply(data: String): PCData =
    new PCData(data.replaceAll("]]>", "]]]]><![CDATA[>"))
  def unapply(other: Any): Option[String] = other match {
    case x: PCData => Some(x.data)
    case _         => None
  }
}
