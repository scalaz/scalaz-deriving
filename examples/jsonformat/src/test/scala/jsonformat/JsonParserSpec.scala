// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import scalaz._, Scalaz._

class JsParserSpec extends WordSpec with JsonTestSupport {

  "The JsParser" should {
    "parse 'null' to JsNull" in {
      JsParser("null").assert_===(JsNull.widen.just)
    }
    "parse 'true' to JsBoolean(true)" in {
      JsParser("true").assert_===(JsBoolean(true).widen.just)
    }
    "parse 'false' to JsBoolean(false)" in {
      JsParser("false").assert_===(JsBoolean(false).widen.just)
    }
    "parse '0' to JsInteger" in {
      JsParser("0").assert_===(JsInteger(0).widen.just)
    }
    "parse '1.23' to JsDouble" in {
      JsParser("1.23").assert_===(JsDouble(1.23).widen.just)
    }
    "double precision is used, not arbitrary precision" in {
      JsParser("-1E10").assert_===(Maybe.empty[JsValue])
    }
    "rounding errors are to be expected" in {
      JsParser("12.34e-10").assert_===(JsDouble(1.234e-9).widen.just)
    }
    "parse \"xyz\" to JsString" in {
      JsParser("\"xyz\"").assert_===(JsString("xyz").widen.just)
    }
    "parse escapes in a JsString" in {
      JsParser(""""\"\\/\b\f\n\r\t"""").assert_===(
        JsString("\"\\/\b\f\n\r\t").widen.just
      )
      JsParser("\"L\\" + "u00e4nder\"")
        .assert_===(JsString("Länder").widen.just)
    }
    "parse all representations of the slash (SOLIDUS) character in a JsString" in {
      JsParser("\"" + "/\\/\\u002f" + "\"")
        .assert_===(JsString("///").widen.just)
    }
    "parse a simple JsObject" in (
      JsParser(""" { "key" :42, "key2": "value" }""").assert_===(
        JsObject("key" -> JsInteger(42), "key2" -> JsString("value")).widen.just
      )
    )
    "parse a simple JsArray" in (
      JsParser("""[null, 1.23 ,{"key":true } ] """).assert_===(
        JsArray(JsNull, JsDouble(1.23), JsObject("key" -> JsBoolean(true))).widen.just
      )
    )
    "be reentrant" in {
      val largeJsonSource = scala.io.Source
        .fromInputStream(getClass.getResourceAsStream("/test.json"))
        .mkString
      List
        .fill(20)(largeJsonSource)
        .map(JsParser(_))
        .toList
        .collect {
          case Maybe.Just(JsObject(fields)) =>
            fields
              .find(_._1 == "questions")
              .collect {
                case (_, arr @ JsArray(_)) => arr
              }
              .get
              .elements
              .length
        }
        .assert_===(List.fill(20)(100))
    }

  }
}
