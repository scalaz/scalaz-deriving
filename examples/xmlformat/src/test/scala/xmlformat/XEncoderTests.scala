// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

import scala.concurrent.duration._

import eu.timepit.refined
import scalaz._, Scalaz._

import org.scalatest.{ Tag => _, _ }
import Matchers._

class XEncoderTests extends FreeSpec {
  import XEncoder.ops._
  import XStrEncoder.ops._

  "XNode Encoder" - {
    "should support Boolean" in {
      true.toXml.shouldBe(XString("true"))
      false.toXml.shouldBe(XString("false"))
    }

    "should support integers" in {
      val expected = XString("13")

      13.toShort.toXml.shouldBe(expected)
      13.toInt.toXml.shouldBe(expected)
      13.toLong.toXml.shouldBe(expected)
    }

    "should support floating point numbers" in {
      val expected = XString("0.1")

      0.1.toFloat.toXml.shouldBe(expected)
      0.1.toDouble.toXml.shouldBe(expected)
    }

    "should support single characters" in {
      'c'.toXml.shouldBe(XString("c"))
    }

    "should support Strings" in {
      "<wibble><wobble".toXml.shouldBe(XString("<wibble><wobble"))
    }

    "should support Symbols" in {
      'foo.toXml.shouldBe(XString("foo"))
    }

    "should special-case Either" in {
      val encoder = XStrEncoder[Either[String, Int]]
      encoder.toXml(Left("hello")).shouldBe(XString("hello"))
      encoder.toXml(Right(13)).shouldBe(XString("13"))
    }

    "should special-case Either for objects" in {
      import examples._

      Stringy("hello")
        .left[Inty]
        .toXml
        .shouldBe(
          XTag("Stringy", XTag("value", XString("hello")).asChild).asChild
        )

      Stringy("hello")
        .right[Inty]
        .toXml
        .shouldBe(
          XTag("Stringy", XTag("value", XString("hello")).asChild).asChild
        )

      StringyTagged("hello")
        .left[Inty]
        .toXml
        .shouldBe(
          XTag("StringyTagged", XTag("value", XString("hello")).asChild).asChild
        )

      StringyTagged("hello")
        .right[Inty]
        .toXml
        .shouldBe(
          XTag("StringyTagged", XTag("value", XString("hello")).asChild).asChild
        )
    }

    "should support Traversables of string content" in {
      val expected = XChildren(
        IList(
          XTag("value", XString("1")),
          XTag("value", XString("2")),
          XTag("value", XString("3"))
        )
      )

      Seq(1, 2, 3).toXml.shouldBe(expected)
      List(1, 2, 3).toXml.shouldBe(expected)
    }

    "should special case Map[Thing, OtherThing]" in {
      Map(1 -> "a", 2 -> "b", 3 -> "c").toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              "entry",
              XChildren(
                IList(
                  XTag("key", XString("1")),
                  XTag("value", XString("a"))
                )
              )
            ),
            XTag(
              "entry",
              XChildren(
                IList(
                  XTag("key", XString("2")),
                  XTag("value", XString("b"))
                )
              )
            ),
            XTag(
              "entry",
              XChildren(
                IList(
                  XTag("key", XString("3")),
                  XTag("value", XString("c"))
                )
              )
            )
          )
        )
      )
    }

    "should support NonEmptyList" in {
      NonEmptyList(1, 2, 3).toXml
        .shouldBe(
          XChildren(
            IList(
              XTag("value", XString("1")),
              XTag("value", XString("2")),
              XTag("value", XString("3"))
            )
          )
        )
    }

    "should support FiniteDuration" in {
      10.seconds.toXml.shouldBe(XString("10000"))
    }

    "should support Instant" in {
      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      instant.toXml.shouldBe(XString(iso))
    }

    "should support refined types" in {
      import refined.api.Refined
      import refined.auto._
      import refined.numeric.Positive
      import refined.scalaz._
      import XStrEncoder.contravariant

      val pos: Long Refined Positive = 1L
      pos.toXml.shouldBe(XString("1"))
    }

    "should support generic products" in {
      import examples._

      Foo("hello").toXml.shouldBe(
        XTag("Foo", XTag("s", XString("hello")).asChild).asChild
      )
      Caz.toXml.shouldBe(
        XTag("Caz.type", XChildren(IList.empty)).asChild
      )
      Baz.toXml.shouldBe(XString("Baz!"))
      Faz(Some("hello")).toXml.shouldBe(
        XTag("Faz", XTag("o", XString("hello")).asChild).asChild
      )

      Faz(None).toXml
        .shouldBe(XTag("Faz", XChildren(IList.empty)).asChild)
    }

    "should support generic coproducts" in {
      import examples._

      (Foo("hello"): SimpleTrait).toXml.shouldBe(
        XTag(
          "SimpleTrait",
          IList(XAttr("typehint", XString("Foo"))),
          IList(XTag("s", XString("hello"))),
          Maybe.empty
        ).asChild
      )
      (Caz: SimpleTrait).toXml.shouldBe(
        XTag(
          "SimpleTrait",
          IList(XAttr("typehint", XString("Caz"))),
          IList.empty,
          Maybe.empty
        ).asChild
      )
      (Baz: SimpleTrait).toXml.shouldBe(
        XTag(
          "SimpleTrait",
          IList(XAttr("typehint", XString("Baz"))),
          IList.empty,
          Maybe.just(XString("Baz!"))
        ).asChild
      )

      (Wobble("fish"): AbstractThing).toXml.shouldBe(
        XTag(
          "AbstractThing",
          IList(XAttr("typehint", XString("Wobble"))),
          IList(XTag("id", XString("fish"))),
          Maybe.empty
        ).asChild
      )

      (Wibble: AbstractThing).toXml.shouldBe(
        XTag(
          "AbstractThing",
          IList(XAttr("typehint", XString("Wibble"))),
          IList.empty,
          Maybe.empty
        ).asChild
      )

      (CoproductInField(Wibble)).toXml.shouldBe(
        XTag(
          "CoproductInField",
          XTag(
            "abs",
            IList(XAttr("typehint", XString("Wibble"))),
            IList.empty,
            Maybe.empty
          ).asChild
        ).asChild
      )
    }

    "should support generic recursive ADTs" in {
      import examples._

      Recursive("hello", Some(Recursive("goodbye"))).toXml.shouldBe(
        XTag(
          "Recursive",
          XChildren(
            IList(
              XTag("h", XString("hello")),
              XTag(
                "t",
                XChildren(
                  IList(
                    XTag("h", XString("goodbye"))
                  )
                )
              )
            )
          )
        ).asChild
      )
    }

    "should encode fields as XmlAttribute" in {
      import examples._

      val multifield = MultiField("hello", Tag("goodbye"))

      multifield.toXml.shouldBe(
        XTag(
          "MultiField",
          IList(XAttr("b", XString("goodbye"))),
          IList(XTag("a", XString("hello"))),
          Maybe.empty
        ).asChild
      )

      (multifield: MultiFieldParent).toXml.shouldBe(
        XTag(
          "MultiFieldParent",
          IList(
            XAttr("typehint", XString("MultiField")),
            XAttr("b", XString("goodbye"))
          ),
          IList(XTag("a", XString("hello"))),
          Maybe.empty
        ).asChild
      )

      MultiOptyField("hello", Tag(Some("goodbye"))).toXml.shouldBe(
        XTag(
          "MultiOptyField",
          IList(XAttr("b", XString("goodbye"))),
          IList(XTag("a", XString("hello"))),
          Maybe.empty
        ).asChild
      )

      MultiOptyField("hello", Tag(None)).toXml
        .shouldBe(
          XTag("MultiOptyField", XTag("a", XString("hello")).asChild).asChild
        )

    }

    "should support lists of tags" in {
      import examples._

      List[AbstractThing](Wibble, Wibble).toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              "AbstractThing",
              IList(XAttr("typehint", XString("Wibble"))),
              IList.empty,
              Maybe.empty
            ),
            XTag(
              "AbstractThing",
              IList(XAttr("typehint", XString("Wibble"))),
              IList.empty,
              Maybe.empty
            )
          )
        )
      )

      List(
        MultiField("hello", XAttribute("goodbye"))
      ).toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              "MultiField",
              IList(XAttr("b", XString("goodbye"))),
              IList(XTag("a", XString("hello"))),
              Maybe.empty
            )
          )
        )
      )
    }

    "should support inlined semigroup fields" in {
      import examples._
      import xmlformat.implicits._

      Inliner(Foo("hello"), "goodbye").toXml.shouldBe(
        XTag(
          "Inliner",
          XChildren(
            IList(
              XTag("Foo", XTag("s", XString("hello")).asChild),
              XTag("nose", XString("goodbye"))
            )
          )
        ).asChild
      )

      InlinerSingle(Foo("hello")).toXml.shouldBe(
        XTag(
          "InlinerSingle",
          XTag("Foo", XTag("s", XString("hello")).asChild).asChild
        ).asChild
      )
    }

    "should support inlined single fields" in {
      import examples._
      import xmlformat.implicits._

      AmbiguousCoproduct(Foo("hello")).toXml.shouldBe(
        XTag(
          "AmbiguousCoproduct",
          XTag(
            "SimpleTrait",
            IList(XAttr("typehint", XString("Foo"))),
            IList(XTag("s", XString("hello"))),
            Maybe.empty
          ).asChild
        ).asChild
      )
    }

    "should support inlined monoid lists" in {
      import examples._
      import xmlformat.implicits._

      Inliners(List(Foo("hello"), Foo("goodbye"))).toXml.shouldBe(
        XTag(
          "Inliners",
          XChildren(
            IList(
              XTag("Foo", XTag("s", XString("hello")).asChild),
              XTag("Foo", XTag("s", XString("goodbye")).asChild)
            )
          )
        ).asChild
      )
    }

    "should support inlined content" in {
      import examples._
      import xmlformat.implicits._

      Outliners(Some("hello"), Some("goodbye")).toXml.shouldBe(
        XTag(
          "Outliners",
          IList(XAttr("id", XString("hello"))),
          IList.empty,
          Maybe.just(XString("goodbye"))
        ).asChild
      )
    }

    "should support tagged coproduct disambiguation" in {
      import examples._

      TaggyCoproduct(XInlined(TaggyA())).toXml.shouldBe(
        XTag("TaggyCoproduct", XTag("TaggyA", XChildren(IList.empty)).asChild).asChild
      )

      TaggyCoproduct(XInlined(TaggyB("hello"))).toXml.shouldBe(
        XTag("TaggyCoproduct", XTag("TaggyB", XString("hello")).asChild).asChild
      )

    }

  }

}
