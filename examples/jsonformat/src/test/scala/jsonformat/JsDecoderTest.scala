// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import JsDecoder.ops._

import scalaz._, Scalaz._

class JsDecoderTest extends JsTest {

  "JsDecoder" should "decode primitives" in {
    JsInteger(42).as[Int].assert_===(\/-(42))
    JsInteger(7563661897011259335L)
      .as[Long]
      .assert_===(\/-(7563661897011259335L))
    JsDouble(4.2).as[Float].assert_===(\/-(4.2f))
    JsDouble(4.2).as[Double].assert_===(\/-(4.2))
    JsInteger(42).as[Byte].assert_===(\/-(42.toByte))
    JsInteger(42).as[Short].assert_===(\/-(42.toShort))
    JsInteger(1).as[Unit].assert_===(\/-(()))
    JsBoolean(true).as[Boolean].assert_===(\/-(true))
    JsBoolean(false).as[Boolean].assert_===(\/-(false))
    JsString("c").as[Char].assert_===(\/-('c'))
    JsString("Hello").as[String].assert_===(\/-("Hello"))
    JsString("Hello").as[Symbol].map(_.name).assert_===(\/-("Hello"))
  }

  it should "give a decent error when decoding fails" in {
    JsString("true")
      .as[Boolean]
      .assert_===(-\/("expected JsBoolean, got JsString(true)"))
  }

  it should "decode Option" in {
    JsNull.as[Option[Int]].assert_===(\/-(None))
    JsString("Hello").as[Option[String]].assert_===(\/-(Some("Hello")))
  }

  it should "decode Either" in {
    JsInteger(42).as[Either[Int, String]].assert_===(\/-(Left(42)))
    JsString("Hello")
      .as[Either[Int, String]]
      .assert_===(\/-(Right("Hello")))

    JsInteger(42)
      .as[Either[Int, Int]]
      .assert_===(-\/("expected No ambiguity, got JsInteger(42)"))
    JsString("42")
      .as[Either[Int, Int]]
      .assert_===(
        -\/(
          "Left: expected JsInteger, got JsString(42)\nRight: expected JsInteger, got JsString(42)"
        )
      )
  }

  it should "decode stringy maps" in {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val json =
      JsObject("a" -> JsInteger(1), "b" -> JsInteger(2), "c" -> JsInteger(3))
    json.as[Map[String, Int]].assert_===(\/-(map))
  }

  it should "decode stdlib CanBuildFromables" in {
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    val list = List(1, 2, 3)
    json.as[List[Int]].assert_===(\/-(list))
    json.as[Set[Int]].assert_===(\/-(Set(1, 2, 3)))
  }

  import examples._
  it should "decode anyval" in {
    JsString("hello").as[Optimal].assert_===(\/-(Optimal("hello")))
  }

  it should "decode generic coproducts" in {
    """{"typehint":"Foo","s":"hello"}"""
      .parseAs[SimpleTrait]
      .assert_===(
        \/-(
          Foo(
            "hello"
          )
        )
      )
    """{"typehint":"Baz"}""".parseAs[SimpleTrait].assert_===(\/-(Baz))

    """{"typehint":"Wibble"}""".parseAs[AbstractThing].assert_===(\/-(Wibble))
    """{"typehint":"Wobble","id":"hello"}"""
      .parseAs[AbstractThing]
      .assert_===(
        \/-(
          Wobble(
            "hello"
          )
        )
      )

    """{"typehint":"Time","xvalue":"goodbye"}"""
      .parseAs[NotAnObject]
      .assert_===(\/-(Time("goodbye")))
    """{"typehint":"Money","i":13}"""
      .parseAs[NotAnObject]
      .assert_===(
        \/-(
          Money(13)
        )
      )
  }

  it should "decode generic recursive ADTs" in {
    """{"h":"hello","t":{"h":"goodbye"}}"""
      .parseAs[Recursive]
      .assert_===(\/-(Recursive("hello", Some(Recursive("goodbye")))))
  }

}
