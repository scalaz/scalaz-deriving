// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import org.scalatest._
import scalaz._, Scalaz._

import JsEncoder.ops._
import JsDecoder.ops._

import scalaz._, Scalaz._

class CollectionFormatsSpec extends WordSpec with JsonTestSupport {

  "The listFormat" should {
    val list = List(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    "convert a List[Int] to a JsArray of JsNumbers" in {
      list.toJson.assert_===(json)
    }
    "convert a JsArray of JsNumbers to a List[Int]" in {
      json.as[List[Int]].assert_===(\/-(list))
    }
  }

  "The mapFormat" should {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsInteger(1), "b" -> JsInteger(2), "c" -> JsInteger(3))
    "convert a Map[String, Long] to a JsObject" in {
      map.toJson.assert_===(json)
    }
    "be able to convert a JsObject to a Map[String, Long]" in {
      json.as[Map[String, Int]].assert_===(\/-(map))
    }
  }

  "The immutableSetFormat" should {
    val set  = Set(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    "convert a Set[Int] to a JsArray of JsIntegers" in {
      set.toJson.assert_===(json)
    }
    "convert a JsArray of JsIntegers to a Set[Int]" in {
      json.as[Set[Int]].assert_===(\/-(set))
    }
  }

  "The indexedSeqFormat" should {
    val seq  = collection.IndexedSeq(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    "convert a Set[Int] to a JsArray of JsIntegers" in {
      seq.toJson.assert_===(json)
    }
    "convert a JsArray of JsIntegers to a IndexedSeq[Int]" in {
      json
        .as[collection.IndexedSeq[Int]]
        .map(_.toList)
        .assert_===(\/-(seq.toList))
    }
  }

}
