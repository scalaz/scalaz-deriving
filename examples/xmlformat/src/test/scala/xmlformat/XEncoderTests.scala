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
      true.toXml.shouldBe(XAtom("true"))
      false.toXml.shouldBe(XAtom("false"))
    }

    "should support integers" in {
      val expected = XAtom("13")

      13.toShort.toXml.shouldBe(expected)
      13.toInt.toXml.shouldBe(expected)
      13.toLong.toXml.shouldBe(expected)
    }

    "should support floating point numbers" in {
      val expected = XAtom("0.1")

      0.1.toFloat.toXml.shouldBe(expected)
      0.1.toDouble.toXml.shouldBe(expected)
    }

    "should support single characters" in {
      'c'.toXml.shouldBe(XText("c"))
    }

    "should support Strings" in {
      "<wibble><wobble".toXml.shouldBe(XText("<wibble><wobble"))
    }

    "should support Symbols" in {
      'foo.toXml.shouldBe(XText("foo"))
    }

    "should special-case Either" in {
      val encoder = XStrEncoder[Either[String, Int]]
      encoder.toXml(Left("hello")).shouldBe(XText("hello"))
      encoder.toXml(Right(13)).shouldBe(XAtom("13"))
    }

    "should special-case Either for objects" in {
      import examples._

      Stringy("hello")
        .left[Inty]
        .toXml
        .shouldBe(
          XTag(
            XAtom("Stringy"),
            XTag(XAtom("value"), XText("hello")).asChild
          ).asChild
        )

      Stringy("hello")
        .right[Inty]
        .toXml
        .shouldBe(
          XTag(
            XAtom("Stringy"),
            XTag(XAtom("value"), XText("hello")).asChild
          ).asChild
        )

      StringyTagged("hello")
        .left[Inty]
        .toXml
        .shouldBe(
          XTag(
            XAtom("StringyTagged"),
            XTag(XAtom("value"), XText("hello")).asChild
          ).asChild
        )

      StringyTagged("hello")
        .right[Inty]
        .toXml
        .shouldBe(
          XTag(
            XAtom("StringyTagged"),
            XTag(XAtom("value"), XText("hello")).asChild
          ).asChild
        )
    }

    "should support Traversables of string content" in {
      val expected = XChildren(
        IList(
          XTag(XAtom("value"), XAtom("1")),
          XTag(XAtom("value"), XAtom("2")),
          XTag(XAtom("value"), XAtom("3"))
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
        )
      )
    }

    "should support NonEmptyList" in {
      NonEmptyList(1, 2, 3).toXml
        .shouldBe(
          XChildren(
            IList(
              XTag(XAtom("value"), XAtom("1")),
              XTag(XAtom("value"), XAtom("2")),
              XTag(XAtom("value"), XAtom("3"))
            )
          )
        )
    }

    "should support FiniteDuration" in {
      10.seconds.toXml.shouldBe(XAtom("10000"))
    }

    "should support Instant" in {
      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      instant.toXml.shouldBe(XAtom(iso))
    }

    "should support refined types" in {
      import refined.api.Refined
      import refined.auto._
      import refined.numeric.Positive
      import refined.scalaz._
      import XStrEncoder.contravariant

      val pos: Long Refined Positive = 1L
      pos.toXml.shouldBe(XAtom("1"))
    }

    "should support generic products" in {
      import examples._

      Foo("hello").toXml.shouldBe(
        XTag(
          XAtom("Foo"),
          XTag(XAtom("s"), XText("hello")).asChild
        ).asChild
      )
      Caz.toXml.shouldBe(
        XTag(XAtom("Caz.type"), XChildren(IList.empty)).asChild
      )
      Baz.toXml.shouldBe(XText("Baz!"))
      Faz(Some("hello")).toXml.shouldBe(
        XTag(XAtom("Faz"), XTag(XAtom("o"), XText("hello")).asChild).asChild
      )

      Faz(None).toXml
        .shouldBe(XTag(XAtom("Faz"), XChildren(IList.empty)).asChild)
    }

    "should support generic coproducts" in {
      import examples._

      (Foo("hello"): SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("SimpleTrait"),
          IList(XAttr(XAtom("typehint"), XAtom("Foo"))),
          IList(XTag(XAtom("s"), XText("hello"))),
          Maybe.empty
        ).asChild
      )
      (Caz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("SimpleTrait"),
          IList(XAttr(XAtom("typehint"), XAtom("Caz"))),
          IList.empty,
          Maybe.empty
        ).asChild
      )
      (Baz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("SimpleTrait"),
          IList(XAttr(XAtom("typehint"), XAtom("Baz"))),
          IList.empty,
          Maybe.just(XText("Baz!"))
        ).asChild
      )

      (Wobble("fish"): AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("AbstractThing"),
          IList(XAttr(XAtom("typehint"), XAtom("Wobble"))),
          IList(XTag(XAtom("id"), XText("fish"))),
          Maybe.empty
        ).asChild
      )

      (Wibble: AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("AbstractThing"),
          IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
          IList.empty,
          Maybe.empty
        ).asChild
      )

      (CoproductInField(Wibble)).toXml.shouldBe(
        XTag(
          XAtom("CoproductInField"),
          XTag(
            XAtom("abs"),
            IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
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
          XAtom("Recursive"),
          XChildren(
            IList(
              XTag(XAtom("h"), XText("hello")),
              XTag(
                XAtom("t"),
                XChildren(
                  IList(
                    XTag(XAtom("h"), XText("goodbye"))
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
          XAtom("MultiField"),
          IList(XAttr(XAtom("b"), XText("goodbye"))),
          IList(XTag(XAtom("a"), XText("hello"))),
          Maybe.empty
        ).asChild
      )

      (multifield: MultiFieldParent).toXml.shouldBe(
        XTag(
          XAtom("MultiFieldParent"),
          IList(
            XAttr(XAtom("typehint"), XAtom("MultiField")),
            XAttr(XAtom("b"), XText("goodbye"))
          ),
          IList(XTag(XAtom("a"), XText("hello"))),
          Maybe.empty
        ).asChild
      )

      MultiOptyField("hello", Tag(Some("goodbye"))).toXml.shouldBe(
        XTag(
          XAtom("MultiOptyField"),
          IList(XAttr(XAtom("b"), XText("goodbye"))),
          IList(XTag(XAtom("a"), XText("hello"))),
          Maybe.empty
        ).asChild
      )

      MultiOptyField("hello", Tag(None)).toXml
        .shouldBe(
          XTag(
            XAtom("MultiOptyField"),
            XTag(XAtom("a"), XText("hello")).asChild
          ).asChild
        )

    }

    "should support lists of tags" in {
      import examples._

      List[AbstractThing](Wibble, Wibble).toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              XAtom("AbstractThing"),
              IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
              IList.empty,
              Maybe.empty
            ),
            XTag(
              XAtom("AbstractThing"),
              IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
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
              XAtom("MultiField"),
              IList(XAttr(XAtom("b"), XText("goodbye"))),
              IList(XTag(XAtom("a"), XText("hello"))),
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
          XAtom("Inliner"),
          XChildren(
            IList(
              XTag(
                XAtom("Foo"),
                XTag(XAtom("s"), XText("hello")).asChild
              ),
              XTag(XAtom("nose"), XText("goodbye"))
            )
          )
        ).asChild
      )

      InlinerSingle(Foo("hello")).toXml.shouldBe(
        XTag(
          XAtom("InlinerSingle"),
          XTag(
            XAtom("Foo"),
            XTag(XAtom("s"), XText("hello")).asChild
          ).asChild
        ).asChild
      )
    }

    "should support inlined single fields" in {
      import examples._
      import xmlformat.implicits._

      AmbiguousCoproduct(Foo("hello")).toXml.shouldBe(
        XTag(
          XAtom("AmbiguousCoproduct"),
          XTag(
            XAtom("SimpleTrait"),
            IList(XAttr(XAtom("typehint"), XAtom("Foo"))),
            IList(XTag(XAtom("s"), XText("hello"))),
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
          XAtom("Inliners"),
          XChildren(
            IList(
              XTag(XAtom("Foo"), XTag(XAtom("s"), XText("hello")).asChild),
              XTag(XAtom("Foo"), XTag(XAtom("s"), XText("goodbye")).asChild)
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
          XAtom("Outliners"),
          IList(XAttr(XAtom("id"), XText("hello"))),
          IList.empty,
          Maybe.just(XText("goodbye"))
        ).asChild
      )
    }

    "should support tagged coproduct disambiguation" in {
      import examples._

      TaggyCoproduct(XInlined(TaggyA())).toXml.shouldBe(
        XTag(
          XAtom("TaggyCoproduct"),
          XTag(
            XAtom("TaggyA"),
            XChildren(IList.empty)
          ).asChild
        ).asChild
      )

      TaggyCoproduct(XInlined(TaggyB("hello"))).toXml.shouldBe(
        XTag(
          XAtom("TaggyCoproduct"),
          XTag(
            XAtom("TaggyB"),
            XText("hello")
          ).asChild
        ).asChild
      )

    }

  }

}
