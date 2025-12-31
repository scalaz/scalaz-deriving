/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat

import JsEncoder.ops.*
import scalaz.*
import scalaz.Scalaz.*

class JsEncoderTest extends JsTest {

  "JsEncoder" should "encode primitives" in {
    42.toJson.assert_===(JsInteger(42))
    7563661897011259335L.toJson.assert_===(JsInteger(7563661897011259335L))
    4.2f.toJson.assert_===(JsDouble(4.2f))
    4.2.toJson.assert_===(JsDouble(4.2))
    42.toByte.toJson.assert_===(JsInteger(42))
    42.toShort.toJson.assert_===(JsInteger(42))
    ().toJson.assert_===(JsInteger(1))
    true.toJson.assert_===(JsBoolean(true))
    false.toJson.assert_===(JsBoolean(false))
    'c'.toJson.assert_===(JsString("c"))
    "Hello".toJson.assert_===(JsString("Hello"))
    Symbol("Hello").toJson.assert_===(JsString("Hello"))
  }

  it should "encode Option" in {
    None.asInstanceOf[Option[Int]].toJson.assert_===(JsNull)
    (Some("Hello"): Option[String]).toJson.assert_===(JsString("Hello"))
  }

  it should "encode Either" in {
    val a: Either[Int, String] = Left(42)
    val b: Either[Int, String] = Right("Hello")

    a.toJson.assert_===(JsInteger(42))
    b.toJson.assert_===(JsString("Hello"))
  }

  // it should "encode Foldables" in {
  //   val ord = OrdSeq(1, 2, 3).self
  //   val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
  //   ord.toJson.assert_===(json)
  //
  //   // is this really what you wanted?
  //   Map(1 -> "foo", 2 -> "bar").toJson.assert_===(
  //     JsArray(JsString("foo"), JsString("bar"))
  //   )
  // }

  it should "encode stringy maps" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsInteger(1), "b" -> JsInteger(2), "c" -> JsInteger(3))

    map.toJson.assert_===(json)
  }

  it should "encode stdlib List" in {
    val list1 = List(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    list1.toJson.assert_===(json)

    val list2 = scala.collection.IndexedSeq(1, 2, 3)
    list2.toJson.assert_===(json)
  }

  import examples.*
  it should "encode anyval" in {
    Optimal("hello").toJson.assert_===(JsString("hello"))
  }

  it should "encode generic coproducts" in {
    Foo("hello").widen.jsonString
      .assert_===("""{"type":"Foo","s":"hello"}""")
    Baz.widen.jsonString.assert_===("""{"type":"Baz"}""")

    Wibble.widen.jsonString.assert_===("""{"type":"Wibble"}""")
    Wobble("hello").widen.jsonString
      .assert_===("""{"type":"Wobble","id":"hello"}""")

    Time("goodbye").widen.jsonString
      .assert_===("""{"TYPE":"Time","xvalue":"goodbye"}""")
    Money(13).widen.jsonString
      .assert_===("""{"TYPE":"Money","integer":13}""")

    Dead("zed's dead").widen.jsonString
      .assert_===("""{"type":"ded","z":"zed's dead"}""")

    Faz(None).widen.jsonString.assert_===("""{"type":"fazzy","o":null}""")
  }

  it should "encode generic recursive ADTs" in {
    val rec = Recursive("hello", Some(Recursive("goodbye")))
    rec.jsonString.assert_===("""{"h":"hello","t":{"h":"goodbye"}}""")
  }

  it should "break the Divide composition law" in {
    val D: Divisible[JsEncoder] = new Divisible[JsEncoder] {
      override def contramap[A, B](fa: JsEncoder[A])(f: B => A): JsEncoder[B] =
        b => fa.toJson(f(b))

      override def divide2[A, B, C](fa: =>JsEncoder[A], fb: =>JsEncoder[B])(
        f: C => (A, B)
      ): JsEncoder[C] = { c =>
        val (a, b) = f(c)
        JsArray(IList(fa.toJson(a), fb.toJson(b)))
      }

      override def conquer[A]: JsEncoder[A] = _ => JsNull
    }

    val S: JsEncoder[String] = JsEncoder[String]
    val E: Equal[JsEncoder[String]] =
      (p1, p2) => p1.toJson("hello") === p2.toJson("hello")
    assert(!D.divideLaw.composition(S, S, S)(E))
  }

}
