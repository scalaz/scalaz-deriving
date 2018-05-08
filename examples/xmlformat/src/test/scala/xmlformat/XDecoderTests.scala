// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

import scala.collection.immutable._
import scala.concurrent.duration._

import scalaz._

import org.scalatest.{ Tag => _, _ }
import eu.timepit.refined
import Matchers._

class XDecoderTests extends FreeSpec {
  import XDecoder.ops._
  import XStrDecoder.ops._
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

    "should special-case Either" in {
      XText("hello").as[Either[String, Int]].shouldBe(Left("hello"))
      XText("foo").as[Either[Int, String]].shouldBe(Right("foo"))

      // the danger of Eithers...
      XText("13")
        .decode[Either[String, Int]]
        .leftValue
        .shouldBe("unable to disambiguate 'XText(13)'")
      XText("13")
        .decode[Either[Int, String]]
        .leftValue
        .shouldBe("unable to disambiguate 'XText(13)'")
    }

    "should special-case Either for objects" in {
      import examples._

      val stringy = XTag(
        XAtom("StringyTagged"),
        XTag(XAtom("value"), XText("hello")).asChild
      ).asChild
      val ambiguous =
        XTag(XAtom("IntyTagged"), XTag(XAtom("value"), XText("13")).asChild).asChild

      stringy.as[Stringy \/ Inty].shouldBe(-\/(Stringy("hello")))
      stringy.as[Inty \/ Stringy].shouldBe(\/-(Stringy("hello")))

      // the danger of Eithers...
      val failure =
        "expected only one branch to succeed, got XChildren([XTag(XAtom(IntyTagged),[],[XTag(XAtom(value),[],[],Just(XText(13)))],Empty())])"
      ambiguous.decode[Stringy \/ Inty].leftValue.shouldBe(failure)
      ambiguous.decode[Inty \/ Stringy].leftValue.shouldBe(failure)

      // but when used with disambiguating tags, it works...
      stringy
        .as[StringyTagged \/ IntyTagged]
        .shouldBe(-\/(StringyTagged("hello")))
      stringy
        .as[IntyTagged \/ StringyTagged]
        .shouldBe(\/-(StringyTagged("hello")))
      ambiguous.as[StringyTagged \/ IntyTagged].shouldBe(\/-(IntyTagged(13)))
      ambiguous.as[IntyTagged \/ StringyTagged].shouldBe(-\/(IntyTagged(13)))
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
            XAtom("name_ignored"),
            XChildren(
              IList(
                XTag(XAtom("key"), XAtom("1")),
                XTag(XAtom("name_ignored"), XText("a"))
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
          // note that the tag names are ignored
          XTag(XAtom("wibble"), XAtom("1")),
          XTag(XAtom("wobble"), XAtom("2")),
          XTag(XAtom("woo"), XAtom("3"))
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

    "should support refined types" in {
      import refined.api.Refined
      import refined.numeric.Positive
      import refined.scalaz._
      import XStrDecoder.monad

      XAtom("-1000")
        .decode[Long Refined Positive]
        .leftValue
        .shouldBe("Predicate failed: (-1000 > 0).")
      XAtom("1000").as[Long Refined Positive].value.shouldBe(1000)
    }

    "should support generic products" in {
      import examples._

      XTag(
        XAtom("Foo"),
        XTag(XAtom("s"), XText("hello")).asChild
      ).asChild
        .as[Foo]
        .shouldBe(
          Foo(
            "hello"
          )
        )
      XTag(XAtom("Caz.type"), XChildren(IList.empty)).asChild
        .as[Caz.type]
        .shouldBe(Caz)
      XText("Baz!").as[Baz.type].shouldBe(Baz)
      XTag(XAtom("Faz"), XTag(XAtom("o"), XText("hello")).asChild).asChild
        .as[Faz]
        .shouldBe(
          Faz(
            Some("hello")
          )
        )

      XText("").decode[Baz.type].leftValue.shouldBe("that's no Baz! ")

      // optional thing
      XTag(XAtom("Faz"), XTag(XAtom("o"), XChildren(IList.empty)).asChild).asChild
        .as[Faz]
        .shouldBe(Faz(None))

      // optional thing, key missing
      XTag(XAtom("Faz"), XChildren(IList.empty)).asChild
        .as[Faz]
        .shouldBe(Faz(None))

    }

    "should support generic coproducts" in {
      import examples._

      XTag(
        XAtom("SimpleTrait"),
        IList(XAttr(XAtom("typehint"), XAtom("Foo"))),
        IList(XTag(XAtom("s"), XText("hello"))),
        Maybe.empty
      ).asChild.as[SimpleTrait].shouldBe(Foo("hello"))

      XTag(
        XAtom("SimpleTrait"),
        IList(XAttr(XAtom("typehint"), XAtom("Caz"))),
        IList.empty,
        Maybe.empty
      ).asChild.as[SimpleTrait].shouldBe(Caz)

      XTag(
        XAtom("SimpleTrait"),
        IList(XAttr(XAtom("typehint"), XAtom("Baz"))),
        IList.empty,
        Maybe.just(XText("Baz!"))
      ).asChild.as[SimpleTrait].shouldBe(Baz)

      XTag(
        XAtom("AbstractThing"),
        IList(XAttr(XAtom("typehint"), XAtom("Wobble"))),
        IList(XTag(XAtom("id"), XText("fish"))),
        Maybe.empty
      ).asChild.as[AbstractThing].shouldBe(Wobble("fish"))

      XTag(
        XAtom("AbstractThing"),
        IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
        IList.empty,
        Maybe.empty
      ).asChild.as[AbstractThing].shouldBe(Wibble)

      XTag(
        XAtom("CoproductInField"),
        XTag(
          XAtom("abs"),
          IList(XAttr(XAtom("typehint"), XAtom("Wibble"))),
          IList.empty,
          Maybe.empty
        ).asChild
      ).asChild.as[CoproductInField].shouldBe(CoproductInField(Wibble))
    }

    "should support generic recursive ADTs" in {
      import examples._

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
        .as[Recursive]
        .shouldBe(Recursive("hello", Some(Recursive("goodbye"))))
    }

    "should decode XmlAttribute fields" in {
      import examples._

      XTag(
        XAtom("MultiField"),
        IList(XAttr(XAtom("b"), XText("goodbye"))),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).as[MultiField].shouldBe(MultiField("hello", Tag("goodbye")))

      XTag(XAtom("MultiOptyField"), XTag(XAtom("a"), XText("hello")).asChild).asChild
        .as[MultiOptyField]
        .shouldBe(MultiOptyField("hello", Tag(None)))

      XTag(
        XAtom("MultiFieldParent"),
        IList(
          XAttr(XAtom("typehint"), XAtom("MultiField")),
          XAttr(XAtom("b"), XText("goodbye"))
        ),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(
          MultiField(
            "hello",
            Tag("goodbye")
          )
        )

      XTag(
        XAtom("MultiFieldParent"),
        IList(
          XAttr(XAtom("typehint"), XAtom("MultiOptyField")),
          XAttr(XAtom("b"), XText("goodbye"))
        ),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(
          MultiOptyField(
            "hello",
            Tag(Some("goodbye"))
          )
        )

      XTag(
        XAtom("MultiFieldParent"),
        IList(
          XAttr(XAtom("typehint"), XAtom("MultiOptyField"))
        ),
        IList(XTag(XAtom("a"), XText("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(MultiOptyField("hello", Tag(None)))
    }

    "should support inlined semigroup fields" in {
      import examples._
      import xmlformat.implicits._

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
      ).asChild.as[Inliner].shouldBe(Inliner(Foo("hello"), "goodbye"))

      XTag(
        XAtom("InlinerSingle"),
        XChildren(
          IList(
            XTag(
              XAtom("Foo"),
              XTag(XAtom("s"), XText("hello")).asChild
            ),
            XTag(
              XAtom("Foo"),
              XTag(XAtom("s"), XText("world")).asChild
            )
          )
        )
      ).asChild.as[InlinerSingle].shouldBe(InlinerSingle(Foo("helloworld")))

      XTag(
        XAtom("InlinerSingle"),
        XChildren(
          IList(
            XTag(
              XAtom("Foo"),
              XTag(XAtom("a"), XText("hello")).asChild
            ),
            XTag(
              XAtom("Foo"),
              XTag(XAtom("b"), XText("world")).asChild
            )
          )
        )
      ).asChild
        .decode[InlinerSingle]
        .leftValue
        .shouldBe(
          """InlinerSingle -> wibble:
Foo -> expected a tag named 's' with a body, got XChildren([XTag(XAtom(Foo),[],[XTag(XAtom(a),[],[],Just(XText(hello)))],Empty())])
Foo -> expected a tag named 's' with a body, got XChildren([XTag(XAtom(Foo),[],[XTag(XAtom(b),[],[],Just(XText(world)))],Empty())])"""
        )
    }

    "should support inlined single fields" in {
      import examples._
      import xmlformat.implicits._

      XTag(
        XAtom("AmbiguousCoproduct"),
        XTag(
          XAtom("SimpleTrait"),
          IList(XAttr(XAtom("typehint"), XAtom("Foo"))),
          IList(XTag(XAtom("s"), XText("hello"))),
          Maybe.empty
        ).asChild
      ).asChild
        .as[AmbiguousCoproduct]
        .shouldBe(
          AmbiguousCoproduct(Foo("hello"))
        )

      // sensible failure messages
      XTag(
        XAtom("AmbiguousCoproduct"),
        XTag(
          XAtom("SimpleTrait"),
          IList(XAttr(XAtom("typehint"), XAtom("Foo"))),
          IList(XTag(XAtom("a"), XText("hello"))),
          Maybe.empty
        ).asChild
      ).asChild
        .decode[AmbiguousCoproduct]
        .leftValue
        .shouldBe(
          """AmbiguousCoproduct -> foo:
SimpleTrait -> Foo -> expected a tag named 's' with a body, got XChildren([XTag(XAtom(SimpleTrait),[XAttr(XAtom(typehint),XAtom(Foo))],[XTag(XAtom(a),[],[],Just(XText(hello)))],Empty())])"""
        )

    }

    "should support inlined monoid lists" in {
      import examples._
      import xmlformat.implicits._

      val xml2 = XTag(
        XAtom("Inliners"),
        XChildren(
          IList(
            XTag(XAtom("Foo"), XTag(XAtom("s"), XText("hello")).asChild),
            XTag(XAtom("Foo"), XTag(XAtom("s"), XText("goodbye")).asChild)
          )
        )
      ).asChild

      xml2.as[Inliners].shouldBe(Inliners(List(Foo("hello"), Foo("goodbye"))))

      val xml3 = XTag(
        XAtom("Inliners"),
        XChildren(
          IList(
            XTag(XAtom("Foo"), XTag(XAtom("s"), XText("hello")).asChild),
            XTag(XAtom("Bar"), XTag(XAtom("s"), XText("goodbye")).asChild),
            XTag(XAtom("Foo"), XTag(XAtom("o"), XText("invisible")).asChild),
            XTag(XAtom("Baz"), XTag(XAtom("s"), XText("oops")).asChild) // note different tag name!
          )
        )
      ).asChild

      xml3
        .as[Inliners]
        .shouldBe(Inliners(List(Foo("hello"), Foo("goodbye"), Foo("oops"))))

      val xml4 = XTag(
        XAtom("Inliners"),
        XChildren(
          IList(
            XTag(XAtom("Foo"), XTag(XAtom("a"), XText("hello")).asChild),
            XTag(XAtom("Bar"), XTag(XAtom("b"), XText("goodbye")).asChild),
            XTag(XAtom("Foo"), XTag(XAtom("c"), XText("invisible")).asChild),
            XTag(XAtom("Baz"), XTag(XAtom("d"), XText("oops")).asChild) // note different tag name!
          )
        )
      ).asChild

      // never fails when there is a Monoid!
      xml4.as[Inliners].shouldBe(Inliners(XInlined(Nil)))
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

    "should support tagged coproduct disambiguation" in {
      import examples._

      XTag(
        XAtom("TaggyCoproduct"),
        XTag(
          XAtom("TaggyA"),
          XChildren(IList.empty)
        ).asChild
      ).asChild
        .as[TaggyCoproduct]
        .shouldBe(TaggyCoproduct(XInlined(TaggyA())))

      XTag(
        XAtom("TaggyCoproduct"),
        XTag(
          XAtom("TaggyB"),
          XText("hello")
        ).asChild
      ).asChild
        .as[TaggyCoproduct]
        .shouldBe(TaggyCoproduct(XInlined(TaggyB("hello"))))

    }

  }

}
