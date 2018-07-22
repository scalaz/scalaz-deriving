// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import Matchers._

import JsDecoder.ops._
import JsEncoder.ops._

import scalaz._, Scalaz._

class BasicFormatsSpec extends WordSpec {

  "The Int formats" should {
    "convert an Int to a JsInteger" in {
      42.toJson.assert_===(JsInteger(42))
    }
    "convert a JsInteger to an Int" in {
      JsInteger(42).as[Int].assert_===(\/-(42))
    }
  }

  "The Long formats" should {
    "convert a Long to a JsInteger" in {
      7563661897011259335L.toJson.assert_===(JsInteger(7563661897011259335L))
    }
    "convert a JsInteger to a Long" in {
      JsInteger(7563661897011259335L)
        .as[Long]
        .assert_===(\/-(7563661897011259335L))
    }
  }

  "The Float formats" should {
    "convert a Float to a JsInteger" in {
      4.2f.toJson.assert_===(JsDouble(4.2f))
    }
    "convert a JsInteger to a Float" in {
      JsDouble(4.2f).as[Float].assert_===(\/-(4.2f))
    }
  }

  "The Double formats" should {
    "convert a Double to a JsInteger" in {
      4.2.toJson.assert_===(JsDouble(4.2))
    }
    "convert a JsInteger to a Double" in {
      JsDouble(4.2).as[Double].assert_===(\/-(4.2))
    }
  }

  "The Byte formats" should {
    "convert a Byte to a JsInteger" in {
      42.asInstanceOf[Byte].toJson.assert_===(JsInteger(42))
    }
    "convert a JsInteger to a Byte" in {
      JsInteger(42).as[Byte].assert_===(\/-(42.toByte))
    }
  }

  "The Short formats" should {
    "convert a Short to a JsInteger" in {
      42.asInstanceOf[Short].toJson.assert_===(JsInteger(42))
    }
    "convert a JsInteger to a Short" in {
      JsInteger(42).as[Short].assert_===(\/-(42.toShort))
    }
  }

  "The Unit formats" should {
    "convert Unit to a JsInteger(1)" in {
      ().toJson.assert_===(JsInteger(1))
    }
    "convert a JsInteger to Unit" in {
      JsInteger(1).as[Unit].assert_===(\/-(()))
    }
  }

  "The Boolean formats" should {
    "convert true to a JsBoolean(true)" in {
      true.toJson.assert_===(JsBoolean(true))
    }
    "convert false to a JsBoolean(false)" in {
      false.toJson.assert_===(JsBoolean(false))
    }
    "convert a JsBoolean(true) to true" in {
      JsBoolean(true).as[Boolean].assert_===(\/-(true))
    }
    "convert a JsBoolean(false) to false" in {
      JsBoolean(false).as[Boolean].assert_===(\/-(false))
    }
  }

  "The Char formats" should {
    "convert a Char to a JsString" in {
      'c'.toJson.assert_===(JsString("c"))
    }
    "convert a JsString to a Char" in {
      JsString("c").as[Char].assert_===(\/-('c'))
    }
  }

  "The String formats" should {
    "convert a String to a JsString" in {
      "Hello".toJson.assert_===(JsString("Hello"))
    }
    "convert a JsString to a String" in {
      JsString("Hello").as[String].assert_===(\/-("Hello"))
    }
  }

  "The Symbol formats" should {
    "convert a Symbol to a JsString" in {
      'Hello.toJson.assert_===(JsString("Hello"))
    }
    "convert a JsString to a Symbol" in {
      JsString("Hello").as[Symbol].map(_.name).assert_===(\/-("Hello"))
    }
  }

}
