// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import Matchers._

import JsEncoder.ops._
import JsDecoder.ops._

import scalaz._, Scalaz._

class StandardFormatsSpec extends WordSpec {

  "The optionFormat" should {
    "convert None to JsNull" in {
      None.asInstanceOf[Option[Int]].toJson.assert_===(JsNull)
    }
    "convert JsNull to None" in {
      JsNull.as[Option[Int]].assert_===(\/-(None))
    }
    "convert Some(Hello) to JsString(Hello)" in {
      Some("Hello")
        .asInstanceOf[Option[String]]
        .toJson
        .assert_===(
          JsString(
            "Hello"
          )
        )
    }
    "convert JsString(Hello) to Some(Hello)" in {
      JsString("Hello").as[Option[String]].assert_===(\/-(Some("Hello")))
    }
  }

  "The eitherFormat" should {
    val a: Either[Int, String] = Left(42)
    val b: Either[Int, String] = Right("Hello")

    "convert the left side of an Either value to Json" in {
      a.toJson.assert_===(JsInteger(42))
    }
    "convert the right side of an Either value to Json" in {
      b.toJson.assert_===(JsString("Hello"))
    }
    "convert the left side of an Either value from Json" in {
      JsInteger(42).as[Either[Int, String]].assert_===(\/-(Left(42)))
    }
    "convert the right side of an Either value from Json" in {
      JsString("Hello")
        .as[Either[Int, String]]
        .assert_===(\/-(Right("Hello")))
    }
  }

}
