// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import JsEncoder.ops._

import scalaz._, Scalaz._

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
    'Hello.toJson.assert_===(JsString("Hello"))
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

  it should "encode Foldables" in {
    val list = List(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    list.toJson.assert_===(json)
  }

  it should "encode stringy maps" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsInteger(1), "b" -> JsInteger(2), "c" -> JsInteger(3))

    map.toJson.assert_===(json)
  }

  it should "encode stdlib CanBuildFrom things" in {
    val set  = Set(1, 2, 3)
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    set.toJson.assert_===(json)

    val seq = collection.IndexedSeq(1, 2, 3)
    seq.toJson.assert_===(json)
  }

  import examples._
  it should "encode anyval" in {
    Optimal("hello").toJson.assert_===(JsString("hello"))
  }

  it should "encode generic coproducts" in {
    (Foo("hello"): SimpleTrait).jsonString
      .assert_===("""{"typehint":"Foo","s":"hello"}""")
    (Baz: SimpleTrait).jsonString.assert_===("""{"typehint":"Baz"}""")

    (Wibble: AbstractThing).jsonString.assert_===("""{"typehint":"Wibble"}""")
    (Wobble("hello"): AbstractThing).jsonString
      .assert_===("""{"typehint":"Wobble","id":"hello"}""")

    (Time("goodbye"): NotAnObject).jsonString
      .assert_===("""{"typehint":"Time","xvalue":"goodbye"}""")
    (Money(13): NotAnObject).jsonString
      .assert_===("""{"typehint":"Money","i":13}""")
  }

  it should "encode generic recursive ADTs" in {
    val rec = Recursive("hello", Some(Recursive("goodbye")))
    rec.jsonString.assert_===("""{"h":"hello","t":{"h":"goodbye"}}""")
  }

  it should "break the Divide composition law" in {
    val D: Divisible[JsEncoder] = new Divisible[JsEncoder] {
      def contramap[A, B](fa: JsEncoder[A])(f: B => A): JsEncoder[B] =
        b => fa.toJson(f(b))

      def divide[A, B, C](fa: JsEncoder[A], fb: JsEncoder[B])(
        f: C => (A, B)
      ): JsEncoder[C] = { c =>
        val (a, b) = f(c)
        JsArray(IList(fa.toJson(a), fb.toJson(b)))
      }

      def conquer[A]: JsEncoder[A] = _ => JsNull
    }

    // may need something more complex than String
    val E: Equal[JsEncoder[String]] =
      (p1, p2) => p1.toJson("hello") === p2.toJson("hello")

    val S: JsEncoder[String] = JsEncoder[String]
    assert(!D.divideLaw.composition(S, S, S)(E))
  }

}
