// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scalaz._, Scalaz._

class PrinterTest extends JsTest {

  "The CompactPrinter" should "print JsNull to 'null'" in {
    CompactPrinter(JsNull).assert_===("null")
  }
  it should "print JsBoolean(true) to 'true'" in {
    CompactPrinter(JsBoolean(true)).assert_===("true")
  }
  it should "print JsBoolean(false) to 'false'" in {
    CompactPrinter(JsBoolean(false)).assert_===("false")
  }
  it should "print JsInteger(0) to '0'" in {
    CompactPrinter(JsInteger(0)).assert_===("0")
  }
  it should "print JsDouble(1.23) to '1.23'" in {
    CompactPrinter(JsDouble(1.23)).assert_===("1.23")
  }
  it should "print JsDouble(-1E10) to '-1E10'" in {
    CompactPrinter(JsDouble(-1e10)).assert_===("-1.0E10")
  }
  it should "print JsDouble(12.34e-10) to '12.34e-10'" in {
    CompactPrinter(JsDouble(12.34e-10)).assert_===("1.234E-9")
  }
  it should "print JsString(\"xyz\") to \"xyz\"" in {
    CompactPrinter(JsString("xyz")).assert_===("\"xyz\"")
  }
  it should "properly escape special chars in JsString" in {
    CompactPrinter(JsString("\"\\\b\f\n\r\t"))
      .assert_===(""""\"\\\b\f\n\r\t"""")
    CompactPrinter(JsString("\u1000")).assert_===("\"\u1000\"")
    CompactPrinter(JsString("\u0100")).assert_===("\"\u0100\"")
    CompactPrinter(JsString("\u0010")).assert_===("\"\\u0010\"")
    CompactPrinter(JsString("\u0001")).assert_===("\"\\u0001\"")
    CompactPrinter(JsString("\u001e")).assert_===("\"\\u001e\"")
    CompactPrinter(JsString("飞机因此受到损伤")).assert_===("\"飞机因此受到损伤\"")
    CompactPrinter(JsString("\uD834\uDD1E")).assert_===("\"\uD834\uDD1E\"")
  }
  it should "properly print a simple JsObject" in (
    CompactPrinter(
      JsObject("key" -> JsInteger(42), "key2" -> JsString("value"))
    ).assert_===("""{"key":42,"key2":"value"}""")
  )
  it should "properly print a simple JsArray" in (
    CompactPrinter(
      JsArray(JsNull, JsDouble(1.23), JsObject("key" -> JsBoolean(true)))
    ).assert_===("""[null,1.23,{"key":true}]""")
  )
  it should "properly print a JSON padding (JSONP) if requested" in {
    CompactPrinter(JsBoolean(true), "customCallback")
      .assert_===("customCallback(true)")
  }

  "The PrettyPrinter" should "align a complicated input" in {
    val Maybe.Just(JsObject(fields)) = JsParser {
      """{
        |  "Boolean no": false,
        |  "Boolean yes":true,
        |  "Unic\u00f8de" :  "Long string with newline\nescape",
        |  "key with \"quotes\"" : "string",
        |  "key with spaces": null,
        |  "number": -1.2323424E-5,
        |  "simpleKey" : "some value",
        |  "sub object" : {
        |    "sub key": 26.5,
        |    "a": "b",
        |    "array": [1, 2, { "yes":1, "no":0 }, ["a", "b", null], false]
        |  },
        |  "zero": 0
        |}""".stripMargin
    }
    PrettyPrinter(JsObject(fields.sortBy(_._1))).assert_=== {
      """{
        |  "Boolean no": false,
        |  "Boolean yes": true,
        |  "Unic\u00f8de": "Long string with newline\nescape",
        |  "key with \"quotes\"": "string",
        |  "key with spaces": null,
        |  "number": -1.2323424E-5,
        |  "simpleKey": "some value",
        |  "sub object": {
        |    "sub key": 26.5,
        |    "a": "b",
        |    "array": [1, 2, {
        |      "yes": 1,
        |      "no": 0
        |    }, ["a", "b", null], false]
        |  },
        |  "zero": 0
        |}""".stripMargin
    }
  }

}
