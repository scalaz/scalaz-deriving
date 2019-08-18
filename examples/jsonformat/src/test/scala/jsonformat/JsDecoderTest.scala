// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import JsDecoder.ops._
import scalaz._, Scalaz._
import scalaz.annotation.deriving

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

  it should "decode stdlib List" in {
    val json = JsArray(JsInteger(1), JsInteger(2), JsInteger(3))
    val list = List(1, 2, 3)
    json.as[List[Int]].assert_===(\/-(list))
  }

  import examples._
  it should "decode anyval" in {
    JsString("hello").as[Optimal].assert_===(\/-(Optimal("hello")))
  }

  it should "decode generic coproducts" in {
    """{"type":"Foo","s":"hello"}"""
      .parseAs[SimpleTrait]
      .assert_===(
        \/-(
          Foo(
            "hello"
          )
        )
      )
    """{"type":"Baz"}""".parseAs[SimpleTrait].assert_===(\/-(Baz))

    """{"type":"Wibble"}""".parseAs[AbstractThing].assert_===(\/-(Wibble))
    """{"type":"Wobble","id":"hello"}"""
      .parseAs[AbstractThing]
      .assert_===(
        \/-(
          Wobble(
            "hello"
          )
        )
      )

    """{"TYPE":"Time","xvalue":"goodbye"}"""
      .parseAs[NotAnObject]
      .assert_===(\/-(Time("goodbye")))
    """{"TYPE":"Money","integer":13}"""
      .parseAs[NotAnObject]
      .assert_===(
        \/-(
          Money(13)
        )
      )

    """{"type":"fazzy","o":null}"""
      .parseAs[SimpleTrait]
      .assert_===(\/-(Faz(None)))
    """{"type":"fazzy"}"""
      .parseAs[SimpleTrait]
      .assert_===(-\/("missing field 'o'"))

    """{"type":"ded","z":"zed's dead"}"""
      .parseAs[Zed]
      .assert_===(\/-(Dead("zed's dead")))
  }

  it should "substitute defaults for missing fields" in {
    """{}""".parseAs[CanHasDefaults].assert_===(\/-(CanHasDefaults("cheez")))
  }

  it should "decode generic recursive ADTs" in {
    """{"h":"hello","t":{"h":"goodbye"}}"""
      .parseAs[Recursive]
      .assert_===(\/-(Recursive("hello", Some(Recursive("goodbye")))))
  }

  it should "treat missing fields as empty" in {
    """{}"""
      .parseAs[Nested]
      .assert_===(\/-(Nested(None)))
  }

  it should "obey the Apply composition law" in {
    composeTest(JsObject(IList("a" -> JsString("hello"), "b" -> JsInteger(1))))
  }

  it should "obey the Apply composition law for bad input" in {
    composeTest(JsNull)
    composeTest(JsObject(IList("a" -> JsString("hello"))))
    composeTest(JsObject(IList("b" -> JsInteger(1))))
  }

  it should "obey the Apply composition law for arbitrary data" in {
    forAll(SizeRange(5))((j: JsValue) => composeTest(j))
  }

  def composeTest(j: JsValue)(implicit P: Position): Assertion = {
    val A                                                = Applicative[JsDecoder]
    val fa: JsDecoder[Comp]                              = JsDecoder[Comp]
    val fab: JsDecoder[Comp => (String, Int)]            = A.point(c => (c.a, c.b))
    val fbc: JsDecoder[((String, Int)) => (Int, String)] = A.point(_.swap)
    val E: Equal[JsDecoder[(Int, String)]] =
      (p1, p2) => p1.fromJson(j) === p2.fromJson(j)
    assert(A.applyLaw.composition(fbc, fab, fa)(E))
  }

}

@deriving(Equal, Show, JsDecoder)
final case class Comp(a: String, b: Int)

@deriving(Equal, Show, JsDecoder)
final case class Nested(n: Option[Nested])
