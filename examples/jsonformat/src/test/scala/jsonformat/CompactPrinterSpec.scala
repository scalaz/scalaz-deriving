// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._

class CompactPrinterSpec extends WordSpec with JsonTestSupport {

  "The CompactPrinter" should {
    "print JsNull to 'null'" in {
      CompactPrinter(JsNull).shouldEqual("null")
    }
    "print JsBoolean(true) to 'true'" in {
      CompactPrinter(JsBoolean(true)).shouldEqual("true")
    }
    "print JsBoolean(false) to 'false'" in {
      CompactPrinter(JsBoolean(false)).shouldEqual("false")
    }
    "print JsInteger(0) to '0'" in {
      CompactPrinter(JsInteger(0)).shouldEqual("0")
    }
    "print JsDouble(1.23) to '1.23'" in {
      CompactPrinter(JsDouble(1.23)).shouldEqual("1.23")
    }
    "print JsDouble(-1E10) to '-1E10'" in {
      CompactPrinter(JsDouble(-1e10)).shouldEqual("-1.0E10")
    }
    "print JsDouble(12.34e-10) to '12.34e-10'" in {
      CompactPrinter(JsDouble(12.34e-10)).shouldEqual("1.234E-9")
    }
    "print JsString(\"xyz\") to \"xyz\"" in {
      CompactPrinter(JsString("xyz")).shouldEqual("\"xyz\"")
    }
    "properly escape special chars in JsString" in {
      CompactPrinter(JsString("\"\\\b\f\n\r\t"))
        .shouldEqual(""""\"\\\b\f\n\r\t"""")
      CompactPrinter(JsString("\u1000")).shouldEqual("\"\u1000\"")
      CompactPrinter(JsString("\u0100")).shouldEqual("\"\u0100\"")
      CompactPrinter(JsString("\u0010")).shouldEqual("\"\\u0010\"")
      CompactPrinter(JsString("\u0001")).shouldEqual("\"\\u0001\"")
      CompactPrinter(JsString("\u001e")).shouldEqual("\"\\u001e\"")
      CompactPrinter(JsString("飞机因此受到损伤")).shouldEqual("\"飞机因此受到损伤\"")
      CompactPrinter(JsString("\uD834\uDD1E")).shouldEqual("\"\uD834\uDD1E\"")
    }
    "properly print a simple JsObject" in (
      CompactPrinter(
        JsObject("key" -> JsInteger(42), "key2" -> JsString("value"))
      ).shouldEqual("""{"key":42,"key2":"value"}""")
    )
    "properly print a simple JsArray" in (
      CompactPrinter(
        JsArray(JsNull, JsDouble(1.23), JsObject("key" -> JsBoolean(true)))
      ).shouldEqual("""[null,1.23,{"key":true}]""")
    )
    "properly print a JSON padding (JSONP) if requested" in {
      CompactPrinter(JsBoolean(true), "customCallback")
        .shouldEqual("customCallback(true)")
    }
  }

}
