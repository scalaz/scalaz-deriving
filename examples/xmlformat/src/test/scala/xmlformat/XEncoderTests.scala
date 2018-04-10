// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

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

    "should support generic products" in {
      import examples._

      Foo("hello").toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("s"), XText("hello")))
        )
      )
      Caz.toXml.shouldBe(XChildren(IList.empty))
      Baz.toXml.shouldBe(XText("Baz!"))
      Faz(Some("hello")).toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("o"), XText("hello")))
        )
      )

      // optional thing (not necessarilly the same as the return value)
      Faz(None).toXml.shouldBe(
        XChildren(
          IList(XTag(XAtom("o"), XChildren(IList.empty)))
        )
      )
    }

    "should support generic coproducts" in {
      import examples._

      (Foo("hello"): SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Foo"),
          XChildren(IList(XTag(XAtom("s"), XText("hello"))))
        )
      )
      (Caz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Caz"),
          XChildren(IList.empty)
        )
      )
      (Baz: SimpleTrait).toXml.shouldBe(
        XTag(
          XAtom("Baz"),
          XText("Baz!")
        )
      )

      (Wobble("fish"): AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("Wobble"),
          XChildren(IList(XTag(XAtom("id"), XText("fish"))))
        )
      )

      (Wibble: AbstractThing).toXml.shouldBe(
        XTag(
          XAtom("Wibble"),
          XChildren(IList.empty)
        )
      )
    }

    "should support generic recursive ADTs" in {
      import examples._

      Recursive("hello", Some(Recursive("goodbye"))).toXml.shouldBe(
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
        )
      )
    }

    "should encode fields as XmlAttribute" in {
      import examples._

      val attributed = XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      )

      val multifield = MultiField("hello", Tag("goodbye"))

      multifield.toXml.shouldBe(attributed)
      (multifield: MultiFieldParent).toXml.shouldBe(attributed)

      MultiOptyField("hello", Tag(Some("goodbye"))).toXml.shouldBe(
        XTag(
          XAtom("MultiOptyField"),
          IList(XAttr(XAtom("b"), XText("goodbye"))),
          IList(XTag(XAtom("a"), XText("hello"))),
          Maybe.empty
        )
      )

      MultiOptyField("hello", Tag(None)).toXml
        .shouldBe(XChildren(IList(XTag(XAtom("a"), XText("hello")))))

    }

    "should support inlined lists" in {
      import examples._

      XInlinedList(List(Wibble, Wibble): List[AbstractThing]).toXml.shouldBe(
        XChildren(
          IList(
            XTag(XAtom("Wibble"), XChildren(IList.empty)),
            XTag(XAtom("Wibble"), XChildren(IList.empty))
          )
        )
      )

      XInlinedList(
        List(
          MultiField("hello", XAttribute("goodbye"))
        )
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

    "should support inlined fields" in {
      import examples._
      import xmlformat.implicits._

      Inliner(Foo("hello"), "goodbye").toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              XAtom("Foo"),
              XChildren(IList(XTag(XAtom("s"), XText("hello"))))
            ),
            XTag(XAtom("nose"), XText("goodbye"))
          )
        )
      )

      InlinerSingle(Foo("hello")).toXml.shouldBe(
        XChildren(
          IList(
            XTag(
              XAtom("Foo"),
              XChildren(IList(XTag(XAtom("s"), XText("hello"))))
            )
          )
        )
      )
    }

    "should support inlined lists that are also inlined fields" in {
      import examples._
      import xmlformat.implicits._

      Inliners(List(Foo("hello"), Foo("goodbye"))).toXml.shouldBe(
        XChildren(
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
        )
      )
    }

  }

}
