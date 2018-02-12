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
      XAtom("true").as[Boolean] shouldBe true
      XAtom("false").as[Boolean] shouldBe false

      XText("true").as[Boolean] shouldBe true
      XText("false").as[Boolean] shouldBe false

      XCdata("true").as[Boolean] shouldBe true
      XCdata("false").as[Boolean] shouldBe false

      XAtom("wibble")
        .decode[Boolean]
        .leftValue shouldBe """expected Boolean in 'wibble'"""
    }

    "should support integers" in {
      XAtom("13").as[Short] shouldBe 13.toShort
      XAtom("13").as[Int] shouldBe 13.toInt
      XAtom("13").as[Long] shouldBe 13.toLong
    }

    "should support floating point numbers" in {
      XAtom("0.1").as[Float] shouldBe 0.1.toFloat
      XAtom("0.1").as[Double] shouldBe 0.1.toDouble
    }

    "should support single characters" in {
      XAtom("ca").decode[Char].leftValue shouldBe "text too long: ca"

      XAtom("c").as[Char] shouldBe 'c'
    }

    "should support Strings" in {
      XText("<wibble><wobble").as[String] shouldBe "<wibble><wobble"
    }

    "should support Symbols" in {
      XText("foo").as[Symbol] shouldEqual 'foo
    }

    "should special-case Option" in {
      XText("hello").as[Option[String]] shouldBe Some("hello")
      XChildren(IList.empty).as[Option[String]] shouldBe None
    }

    "should special-case Either" in {
      XText("hello").as[Either[String, Int]] shouldBe Left("hello")

      // NOTE! we wanted the Right but we got a Left because it is valid and
      // preferred
      XText("13").as[Either[String, Int]] shouldBe Left("13")

      XText("13").as[Either[Int, String]] shouldBe Left(13)
      XText("foo").as[Either[Int, String]] shouldBe Right("foo")
    }

    "should support Traversables" in {
      val xml = XChildren(
        IList(
          XTag(XAtom("value"), IList.empty, XAtom("1")),
          XTag(XAtom("value"), IList.empty, XAtom("2")),
          XTag(XAtom("value"), IList.empty, XAtom("3"))
        )
      )

      xml.as[List[Int]] shouldBe List(1, 2, 3)
      xml.as[Seq[Int]] shouldBe Seq(1, 2, 3)
      xml.as[Set[Int]] shouldBe Set(1, 2, 3)

      // because ListSet hates us
      XChildren(
        IList(
          XTag(XAtom("value"), IList.empty, XAtom("3")),
          XTag(XAtom("value"), IList.empty, XAtom("2")),
          XTag(XAtom("value"), IList.empty, XAtom("1"))
        )
      ).as[ListSet[Int]] shouldBe ListSet(1, 2, 3)

      // sometimes single element things come through like this
      XTag(XAtom("value"), IList.empty, XAtom("1")).as[List[Int]] shouldBe List(
        1
      )

      // what about empty lists?
    }

    "should special case Map[Thing, OtherThing]" in {
      XChildren(
        IList(
          XTag(
            XAtom("entry"),
            IList.empty,
            XChildren(
              IList(
                XTag(XAtom("key"), IList.empty, XAtom("1")),
                XTag(XAtom("value"), IList.empty, XText("a"))
              )
            )
          ),
          XTag(
            XAtom("entry"),
            IList.empty,
            XChildren(
              IList(
                XTag(XAtom("key"), IList.empty, XAtom("2")),
                XTag(XAtom("value"), IList.empty, XText("b"))
              )
            )
          ),
          XTag(
            XAtom("entry"),
            IList.empty,
            XChildren(
              IList(
                XTag(XAtom("key"), IList.empty, XAtom("3")),
                XTag(XAtom("value"), IList.empty, XText("c"))
              )
            )
          )
        )
      ).as[Map[Int, String]] shouldBe Map(1 -> "a", 2 -> "b", 3 -> "c")
    }

    "should support NonEmptyList" in {
      XChildren(
        IList(
          XTag(XAtom("value"), IList.empty, XAtom("1")),
          XTag(XAtom("value"), IList.empty, XAtom("2")),
          XTag(XAtom("value"), IList.empty, XAtom("3"))
        )
      ).as[NonEmptyList[Int]] shouldBe NonEmptyList.nels(1, 2, 3)

      XChildren(IList.empty)
        .decode[NonEmptyList[Int]]
        .leftValue shouldBe "list was empty"
    }

    "should support FiniteDuration" in {
      XText("10000").as[FiniteDuration] shouldBe 10.seconds
    }

    "should support Instant" in {
      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      XAtom(iso).as[Instant] shouldBe instant
    }

    "should support generic products" in {
      import examples._

      XChildren(IList(XTag(XAtom("s"), IList.empty, XText("hello"))))
        .as[Foo] shouldBe Foo(
        "hello"
      )
      XChildren(IList.empty).as[Caz.type] shouldBe Caz
      XText("Baz!").as[Baz.type] shouldBe Baz
      XChildren(IList(XTag(XAtom("o"), IList.empty, XText("hello"))))
        .as[Faz] shouldBe Faz(
        Some("hello")
      )

      XText("")
        .decode[Foo]
        .leftValue shouldBe "when decoding xmlformat.examples.Foo: missing tag s"

      XText("").decode[Baz.type].leftValue shouldBe "that's no Baz! "

      // optional thing
      XChildren(IList(XTag(XAtom("o"), IList.empty, XChildren(IList.empty))))
        .as[Faz] shouldBe Faz(
        None
      )
      // optional thing, key missing
      XChildren(IList.empty).as[Faz] shouldBe Faz(None)

      // that ambiguous single element list again...
      XTag(XAtom("s"), IList.empty, XText("hello")).as[Foo] shouldBe Foo(
        "hello"
      )
    }

    "should support generic coproducts" in {
      import examples._

      XTag(
        XAtom("Foo"),
        IList.empty,
        XChildren(IList(XTag(XAtom("s"), IList.empty, XText("hello"))))
      ).as[SimpleTrait] shouldBe Foo("hello")

      XTag(XAtom("Caz"), IList.empty, XChildren(IList.empty))
        .as[SimpleTrait] shouldBe Caz
      XTag(XAtom("Baz"), IList.empty, XText("Baz!"))
        .as[SimpleTrait] shouldBe Baz

      XTag(
        XAtom("Wobble"),
        IList.empty,
        XChildren(IList(XTag(XAtom("id"), IList.empty, XText("fish"))))
      ).as[AbstractThing] shouldBe Wobble("fish")

      XTag(
        XAtom("Wibble"),
        IList.empty,
        XChildren(IList.empty)
      ).as[AbstractThing] shouldBe Wibble

      XText("")
        .decode[SimpleTrait]
        .leftValue shouldBe "when decoding xmlformat.examples.SimpleTrait: unexpected []"

      // sometimes XTags come through as XChildren with one element
      XChildren(
        IList(XTag(XAtom("Wibble"), IList.empty, XChildren(IList.empty)))
      ).as[Wibble.type] shouldBe Wibble

      XChildren(
        IList(XTag(XAtom("Wibble"), IList.empty, XChildren(IList.empty)))
      ).as[AbstractThing] shouldBe Wibble
    }

    "should support generic recursive ADTs" in {
      import examples._

      XChildren(
        IList(
          XTag(XAtom("h"), IList.empty, XText("hello")),
          XTag(
            XAtom("t"),
            IList.empty,
            XChildren(
              IList(
                XTag(XAtom("h"), IList.empty, XText("goodbye")),
                XTag(XAtom("t"), IList.empty, XChildren(IList.empty))
              )
            )
          )
        )
      ).as[Recursive] shouldBe Recursive("hello", Some(Recursive("goodbye")))
    }

    "should decode XmlAttribute fields" in {
      import examples._

      XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        XChildren(
          IList(XTag(XAtom("a"), IList.empty, XText("hello").asContent))
        )
      ).as[MultiField] shouldBe MultiField("hello", Tag("goodbye"))

      XChildren(IList(XTag(XAtom("a"), IList.empty, XText("hello"))))
        .as[MultiOptyField] shouldBe MultiOptyField("hello", Tag(None))

      XTag(
        XAtom("MultiOptyField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        XChildren(IList(XTag(XAtom("a"), IList.empty, XText("hello"))))
      ).as[MultiFieldParent] shouldBe MultiOptyField(
        "hello",
        Tag(Some("goodbye"))
      )

      XTag(
        XAtom("MultiOptyField"),
        IList.empty,
        XChildren(IList(XTag(XAtom("a"), IList.empty, XText("hello"))))
      ).as[MultiFieldParent] shouldBe MultiOptyField("hello", Tag(None))
    }

  }

}
