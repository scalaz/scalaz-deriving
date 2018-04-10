// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

import scala.collection.immutable._
import scala.concurrent.duration._

import scalaz._

import org.scalatest.{ Tag => _, _ }
import Matchers._

class XDecoderTests extends FreeSpec {
  import XDecoder.ops._
  import XTestUtils._

  "XNode Decoder" - {

    "should support Boolean" in {
      XAtom("true").as[Boolean].shouldBe(true)
      XAtom("false").as[Boolean].shouldBe(false)

      XText("true").as[Boolean].shouldBe(true)
      XText("false").as[Boolean].shouldBe(false)

      XCdata("true").as[Boolean].shouldBe(true)
      XCdata("false").as[Boolean].shouldBe(false)

      XAtom("wibble")
        .decode[Boolean]
        .leftValue
        .shouldBe("""expected Boolean in 'wibble'""")
    }

    "should support integers" in {
      XAtom("13").as[Short].shouldBe(13.toShort)
      XAtom("13").as[Int].shouldBe(13.toInt)
      XAtom("13").as[Long].shouldBe(13.toLong)
    }

    "should support floating point numbers" in {
      XAtom("0.1").as[Float].shouldBe(0.1.toFloat)
      XAtom("0.1").as[Double].shouldBe(0.1.toDouble)
    }

    "should support single characters" in {
      XAtom("ca").decode[Char].leftValue.shouldBe("text too long: ca")

      XAtom("c").as[Char].shouldBe('c')
    }

    "should support Strings" in {
      XText("<wibble><wobble").as[String].shouldBe("<wibble><wobble")
    }

    "should support Symbols" in {
      XText("foo").as[Symbol].shouldEqual('foo)
    }

    "should special-case Option" in {
      XText("hello").as[Option[String]].shouldBe(Some("hello"))
      XChildren(IList.empty).as[Option[String]].shouldBe(None)
    }

    "should special-case Either" in {
      XText("hello").as[Either[String, Int]].shouldBe(Left("hello"))

      // NOTE! we wanted the Right but we got a Left because it is valid and
      // preferred
      XText("13").as[Either[String, Int]].shouldBe(Left("13"))

      XText("13").as[Either[Int, String]].shouldBe(Left(13))
      XText("foo").as[Either[Int, String]].shouldBe(Right("foo"))
    }

    "should support Traversables" in {
      val xml = XChildren(
        IList(
          XTag(XAtom("value"), XAtom("1")),
          XTag(XAtom("value"), XAtom("2")),
          XTag(XAtom("value"), XAtom("3"))
        )
      )

      xml.as[List[Int]].shouldBe(List(1, 2, 3))
      xml.as[Seq[Int]].shouldBe(Seq(1, 2, 3))
      xml.as[Set[Int]].shouldBe(Set(1, 2, 3))

      // sometimes single element things come through like this
      XTag(XAtom("value"), XAtom("1")).as[List[Int]].shouldBe(List(1))

      // what about empty lists?
    }

    "should special case Map[Thing, OtherThing]" in {
      XChildren(
        IList(
          XTag(
            XAtom("entry"),
            XChildren(
              IList(
                XTag(XAtom("key"), XAtom("1")),
                XTag(XAtom("value"), XText("a"))
              )
            )
          ),
          XTag(
            XAtom("entry"),
            XChildren(
              IList(
                XTag(XAtom("key"), XAtom("2")),
                XTag(XAtom("value"), XText("b"))
              )
            )
          ),
          XTag(
            XAtom("entry"),
            XChildren(
              IList(
                XTag(XAtom("key"), XAtom("3")),
                XTag(XAtom("value"), XText("c"))
              )
            )
          )
        )
      ).as[Map[Int, String]].shouldBe(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    }

    "should support NonEmptyList" in {
      XChildren(
        IList(
          XTag(XAtom("value"), XAtom("1")),
          XTag(XAtom("value"), XAtom("2")),
          XTag(XAtom("value"), XAtom("3"))
        )
      ).as[NonEmptyList[Int]]
        .shouldBe(
          NonEmptyList(1, 2, 3)
        )

      XChildren(IList.empty)
        .decode[NonEmptyList[Int]]
        .leftValue
        .shouldBe("list was empty")
    }

    "should support FiniteDuration" in {
      XText("10000").as[FiniteDuration].shouldBe(10.seconds)
    }

    "should support Instant" in {
      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      XAtom(iso).as[Instant].shouldBe(instant)
    }

    "should support generic products" in {
      import examples._

      XChildren(IList(XTag(XAtom("s"), XText("hello"))))
        .as[Foo]
        .shouldBe(
          Foo(
            "hello"
          )
        )
      XChildren(IList.empty).as[Caz.type].shouldBe(Caz)
      XText("Baz!").as[Baz.type].shouldBe(Baz)
      XChildren(IList(XTag(XAtom("o"), XText("hello"))))
        .as[Faz]
        .shouldBe(
          Faz(
            Some("hello")
          )
        )

      XText("")
        .decode[Foo]
        .leftValue
        .shouldBe("Foo -> missing tag 's'")

      XText("").decode[Baz.type].leftValue.shouldBe("that's no Baz! ")

      // optional thing
      XChildren(IList(XTag(XAtom("o"), XChildren(IList.empty))))
        .as[Faz]
        .shouldBe(Faz(None))
      // optional thing, key missing
      XChildren(IList.empty).as[Faz].shouldBe(Faz(None))

      // that ambiguous single element list again...
      XTag(XAtom("s"), XText("hello")).as[Foo].shouldBe(Foo("hello"))
    }

    "should support generic coproducts" in {
      import examples._

      XTag(
        XAtom("Foo"),
        XChildren(IList(XTag(XAtom("s"), XText("hello"))))
      ).as[SimpleTrait].shouldBe(Foo("hello"))

      XTag(XAtom("Caz"), XChildren(IList.empty)).as[SimpleTrait].shouldBe(Caz)
      XTag(XAtom("Baz"), XText("Baz!")).as[SimpleTrait].shouldBe(Baz)

      XTag(
        XAtom("Wobble"),
        XChildren(IList(XTag(XAtom("id"), XText("fish"))))
      ).as[AbstractThing].shouldBe(Wobble("fish"))

      XTag(
        XAtom("Wibble"),
        XChildren(IList.empty)
      ).as[AbstractThing].shouldBe(Wibble)

      XText("")
        .decode[SimpleTrait]
        .leftValue
        .shouldBe("SimpleTrait -> unexpected []...")

      // sometimes XTags come through as XChildren with one element
      XChildren(
        IList(XTag(XAtom("Wibble"), XChildren(IList.empty)))
      ).as[Wibble.type].shouldBe(Wibble)

      XChildren(
        IList(XTag(XAtom("Wibble"), XChildren(IList.empty)))
      ).as[AbstractThing].shouldBe(Wibble)
    }

    "should support generic recursive ADTs" in {
      import examples._

      XChildren(
        IList(
          XTag(XAtom("h"), XText("hello")),
          XTag(
            XAtom("t"),
            XChildren(
              IList(
                XTag(XAtom("h"), XText("goodbye")),
                XTag(XAtom("t"), XChildren(IList.empty))
              )
            )
          )
        )
      ).as[Recursive].shouldBe(Recursive("hello", Some(Recursive("goodbye"))))
    }

    "should decode XmlAttribute fields" in {
      import examples._

      XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).as[MultiField].shouldBe(MultiField("hello", Tag("goodbye")))

      XChildren(IList(XTag(XAtom("a"), XText("hello"))))
        .as[MultiOptyField]
        .shouldBe(MultiOptyField("hello", Tag(None)))

      XTag(
        XAtom("MultiOptyField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).as[MultiFieldParent]
        .shouldBe(
          MultiOptyField(
            "hello",
            Tag(Some("goodbye"))
          )
        )

      XTag(
        XAtom("MultiOptyField"),
        XChildren(IList(XTag(XAtom("a"), XText("hello"))))
      ).as[MultiFieldParent].shouldBe(MultiOptyField("hello", Tag(None)))
    }

    "should support inlined lists" in {
      import examples._

      val xml2 = XChildren(
        IList(
          XTag(XAtom("Wibble"), XChildren(IList.empty)),
          XTag(XAtom("Wibble"), XChildren(IList.empty))
        )
      )

      xml2.as[List[Wibble.type] @@ XInlinedList].shouldBe(List(Wibble, Wibble))

      // sometimes single element things come through like this
      val xml1 = XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      )

      xml1
        .as[List[MultiField] @@ XInlinedList]
        .shouldBe(
          List(
            MultiField("hello", XAttribute("goodbye"))
          )
        )
    }

    "should support inlined fields" in {
      import examples._
      import xmlformat.implicits._

      val xml2 = XChildren(
        IList(
          XTag(XAtom("nose"), XText("goodbye")),
          XTag(
            XAtom("Foo"),
            XChildren(IList(XTag(XAtom("s"), XText("hello"))))
          )
        )
      )

      xml2.as[Inliner].shouldBe(Inliner(Foo("hello"), "goodbye"))

      // that ambiguous single element list again...
      val xml1 = XTag(
        XAtom("Foo"),
        XChildren(IList(XTag(XAtom("s"), XText("hello"))))
      )

      xml1.as[InlinerSingle].shouldBe(InlinerSingle(Foo("hello")))
    }

    "should fail to decode nested single elements of the same name" in {
      import examples._

      val xml = XTag(
        XAtom("Foo"),
        XChildren(
          IList(
            XTag(
              XAtom("Foo"),
              XChildren(IList(XTag(XAtom("s"), XText("hello"))))
            )
          )
        )
      )

      xml
        .decode[NestedSingle]
        .leftValue
        .shouldBe("""NestedSingle -> unexpected 2 elements named 'Foo'""")
    }

    "should support inlined lists that are also inlined fields" in {
      import examples._
      import xmlformat.implicits._

      val xml2 = XChildren(
        IList(
          XTag(
            XAtom("Foo"),
            XChildren(IList(XTag(XAtom("s"), XText("hello"))))
          ),
          XTag(
            XAtom("Foo"),
            XChildren(IList(XTag(XAtom("s"), XText("goodbye"))))
          )
        )
      )

      xml2.as[Inliners].shouldBe(Inliners(List(Foo("hello"), Foo("goodbye"))))
    }

    "should support inlined content" in {
      import examples._
      import xmlformat.implicits._

      val xml1 = XTag(
        XAtom("Outliners"),
        IList(XAttr(XAtom("id"), XText("hello"))),
        IList.empty,
        Maybe.just(XText("goodbye"))
      )

      xml1.as[Outliners].shouldBe(Outliners(Some("hello"), Some("goodbye")))

      val xml2 = XTag(
        XAtom("Outliners"),
        IList(XAttr(XAtom("id"), XText("hello"))),
        IList.empty,
        Maybe.empty
      )

      xml2.as[Outliners].shouldBe(Outliners(Some("hello"), None))

      val xml3 = XTag(
        XAtom("Outliners"),
        IList.empty,
        IList.empty,
        Maybe.just(XText("goodbye"))
      )

      xml3.as[Outliners].shouldBe(Outliners(None, Some("goodbye")))
    }

  }

}
