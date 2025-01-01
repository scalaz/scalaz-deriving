// Copyright: 2017 - 2025 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.util.control.NoStackTrace
import org.typelevel.jawn._
import scalaz._, Scalaz._
import internal.FastToIList._

object JsParser extends SupportParser[JsValue] {

  // jawn uses exceptions for control flow (the error case), which is a huge DOS
  // security vulnerability, but we can't do anything about it.

  def apply(s: String): String \/ JsValue =
    Maybe.attempt(parseUnsafe(s)) \/> "invalid json"

  implicit val facade: Facade[JsValue] =
    new Facade.SimpleFacade[JsValue] {
      val jnull: JsNull.type                                            = JsNull
      val jfalse: JsBoolean                                             = JsBoolean(false)
      val jtrue: JsBoolean                                              = JsBoolean(true)
      def jnum(cs: CharSequence, decIndex: Int, expIndex: Int): JsValue = {
        val s = cs.toString
        val n =
          if (decIndex == -1)
            s.parseLong.map(JsInteger(_))
          else if (s.endsWith(".0"))
            s.substring(0, s.length - 2).parseLong.map(JsInteger(_))
          else
            s.parseDouble.map(JsDouble(_))
        n.getOrElse(
          throw new IllegalArgumentException(s"bad number $s")
            with NoStackTrace // scalafix:ok
        )
      }

      def jstring(s: CharSequence): JsString          = JsString(s.toString)
      def jarray(vs: List[JsValue]): JsArray          = JsArray(vs.toIList)
      def jobject(vs: Map[String, JsValue]): JsObject = JsObject(vs.asIList)
    }
}
