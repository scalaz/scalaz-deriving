// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._

import scalaz._, Scalaz._

class PrettyPrinterSpec extends WordSpec with JsonTestSupport {

  "The PrettyPrinter" should {
    "print a more complicated JsObject nicely aligned" in {
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

}
