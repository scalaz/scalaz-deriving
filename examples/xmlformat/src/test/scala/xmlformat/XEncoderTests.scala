// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

import scalaz._, Scalaz._

import org.scalatest.{ Tag => _, _ }
import Matchers._

class XEncoderTests extends FreeSpec {
  import XEncoder.ops._

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

    "should special-case Option" in {
      (Some("hello"): Option[String]).toXml.shouldBe(XText("hello"))

      (None: Option[String]).toXml.shouldBe(XChildren(IList.empty))
    }

    "should special-case Either" in {
      (Left("hello"): Either[String, Int]).toXml.shouldBe(XText("hello"))

      (Right(13): Either[String, Int]).toXml.shouldBe(XAtom("13"))
    }

    "should support Traversables" in {
      val expected = XChildren(
        IList(
          XTag(XAtom("value"), IList.empty, XAtom("1")),
          XTag(XAtom("value"), IList.empty, XAtom("2")),
          XTag(XAtom("value"), IList.empty, XAtom("3"))
        )
      )

      val legacy = XChildren(
        IList(
          XTag(XAtom("value"), IList.empty, XAtom("3")),
          XTag(XAtom("value"), IList.empty, XAtom("2")),
          XTag(XAtom("value"), IList.empty, XAtom("1"))
        )
      )

      Seq(1, 2, 3).toXml.shouldBe(expected)
      ListSet(1, 2, 3).toXml should (be(expected).or(be(legacy)))
      List(1, 2, 3).toXml.shouldBe(expected)
    }

    "should special case Map[Thing, OtherThing]" in {
      Map(1 -> "a", 2 -> "b", 3 -> "c").toXml.shouldBe(
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
        )
      )
    }

    "should support NonEmptyList" in {
      NonEmptyList
        .nels(1, 2, 3)
        .toXml
        .shouldBe(
          XChildren(
            IList(
              XTag(XAtom("value"), IList.empty, XAtom("1")),
              XTag(XAtom("value"), IList.empty, XAtom("2")),
              XTag(XAtom("value"), IList.empty, XAtom("3"))
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

    "should support generic products" in {
      import examples._

      Foo("hello").toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("s"), IList.empty, XText("hello")))
        )
      )
      Caz.toXml.shouldBe(XChildren(IList.empty))
      Baz.toXml.shouldBe(XText("Baz!"))
      Faz(Some("hello")).toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("o"), IList.empty, XText("hello")))
        )
      )

      // optional thing (not necessarilly the same as the return value)
      Faz(None).toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("o"), IList.empty, XChildren(IList.empty)))
        )
      )
    }

    "should support generic coproducts" in {
      import examples._

      (Foo("hello"): SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Foo"),
          IList.empty,
          XChildren(IList(XTag(XAtom("s"), IList.empty, XText("hello"))))
        )
      )
      (Caz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Caz"),
          IList.empty,
          XChildren(IList.empty)
        )
      )
      (Baz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Baz"),
          IList.empty,
          XText("Baz!")
        )
      )

      (Wobble("fish"): AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("Wobble"),
          IList.empty,
          XChildren(IList(XTag(XAtom("id"), IList.empty, XText("fish"))))
        )
      )

      (Wibble: AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("Wibble"),
          IList.empty,
          XChildren(IList.empty)
        )
      )
    }

    "should support generic recursive ADTs" in {
      import examples._

      Recursive("hello", Some(Recursive("goodbye"))).toXml.shouldBe(
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
        )
      )
    }

    "should encode fields as XmlAttribute" in {
      import examples._

      val attributed = XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        XChildren(
          IList(XTag(XAtom("a"), IList.empty, XText("hello").asContent))
        )
      )

      val multifield = MultiField("hello", Tag("goodbye"))

      multifield.toXml.shouldBe(attributed)
      (multifield: MultiFieldParent).toXml.shouldBe(attributed)

      MultiOptyField("hello", Tag(Some("goodbye"))).toXml.shouldBe(
        XTag(
          XAtom("MultiOptyField"),
          IList(XAttr(XAtom("b"), XText("goodbye"))),
          XChildren(IList(XTag(XAtom("a"), IList.empty, XText("hello"))))
        )
      )

      MultiOptyField("hello", Tag(None)).toXml.shouldBe(
        XChildren(IList(XTag(XAtom("a"), IList.empty, XText("hello"))))
      )

    }
  }

}
