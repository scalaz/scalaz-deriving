// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.time.Instant

import scala.collection.immutable._
import scala.concurrent.duration._

import scalaz._

import org.scalatest._
import eu.timepit.refined
import Matchers._

class XDecoderTests extends FreeSpec {
  import XDecoder.ops._
  import XStrDecoder.ops._
  import XTestUtils._

  "XNode Decoder" - {

    "should support Boolean" in {
      XString("true").as[Boolean].shouldBe(true)
      XString("false").as[Boolean].shouldBe(false)

      XString("wibble")
        .decode[Boolean]
        .leftValue
        .shouldBe("""expected Boolean in 'wibble'""")
    }

    "should support integers" in {
      XString("13").as[Short].shouldBe(13.toShort)
      XString("13").as[Int].shouldBe(13.toInt)
      XString("13").as[Long].shouldBe(13.toLong)
    }

    "should support floating point numbers" in {
      XString("0.1").as[Float].shouldBe(0.1.toFloat)
      XString("0.1").as[Double].shouldBe(0.1.toDouble)
    }

    "should support single characters" in {
      XString("ca").decode[Char].leftValue.shouldBe("text too long: ca")

      XString("c").as[Char].shouldBe('c')
    }

    "should support Strings" in {
      XString("<wibble><wobble").as[String].shouldBe("<wibble><wobble")
    }

    "should support Symbols" in {
      XString("foo").as[Symbol].shouldEqual(Symbol("foo"))
    }

    "should special-case Either" in {
      XString("hello").as[Either[String, Int]].shouldBe(Left("hello"))
      XString("foo").as[Either[Int, String]].shouldBe(Right("foo"))

      // the danger of Eithers...
      XString("13")
        .decode[Either[String, Int]]
        .leftValue
        .shouldBe("unable to disambiguate 'XString(13)'")
      XString("13")
        .decode[Either[Int, String]]
        .leftValue
        .shouldBe("unable to disambiguate 'XString(13)'")
    }

    "should special-case Either for objects" in {
      import examples._

      val stringy = XTag(
        "StringyTagged",
        XTag("value", XString("hello")).asChild
      ).asChild
      val ambiguous =
        XTag("IntyTagged", XTag("value", XString("13")).asChild).asChild

      stringy.as[Stringy \/ Inty].shouldBe(-\/(Stringy("hello")))
      stringy.as[Inty \/ Stringy].shouldBe(\/-(Stringy("hello")))

      // the danger of Eithers...
      val failure =
        "expected only one branch to succeed, got XChildren([XTag(IntyTagged,[],[XTag(value,[],[],Just(XString(13)))],Empty())])"
      ambiguous.decode[Stringy \/ Inty].leftValue.shouldBe(failure)
      ambiguous.decode[Inty \/ Stringy].leftValue.shouldBe(failure)

      val bad = XTag(
        "StringyTagged",
        XTag("not-a-value", XString("hello")).asChild
      ).asChild
      bad
        .decode[Stringy \/ Inty]
        .leftValue
        .shouldBe(
          """expected one branch to succeed, got:
Left: Stringy -> expected one 'value' with a body, got XChildren([XTag(StringyTagged,[],[XTag(not-a-value,[],[],Just(XString(hello)))],Empty())])
Right: Inty -> expected one 'value' with a body, got XChildren([XTag(StringyTagged,[],[XTag(not-a-value,[],[],Just(XString(hello)))],Empty())])"""
        )

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
          XTag("value", XString("1")),
          XTag("value", XString("2")),
          XTag("value", XString("3"))
        )
      )

      xml.as[List[Int]].shouldBe(List(1, 2, 3))
      xml.as[Seq[Int]].shouldBe(Seq(1, 2, 3))
      xml.as[Set[Int]].shouldBe(Set(1, 2, 3))

      // sometimes single element things come through like this
      XTag("value", XString("1")).as[List[Int]].shouldBe(List(1))

      // what about empty lists?
    }

    "should special case Map[Thing, OtherThing]" in {
      XChildren(
        IList(
          XTag(
            "name_ignored",
            XChildren(
              IList(
                XTag("key", XString("1")),
                XTag("name_ignored", XString("a"))
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
      ).as[Map[Int, String]].shouldBe(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    }

    "should support NonEmptyList" in {
      XChildren(
        IList(
          // note that the tag names are ignored
          XTag("wibble", XString("1")),
          XTag("wobble", XString("2")),
          XTag("woo", XString("3"))
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
      XString("10000").as[FiniteDuration].shouldBe(10.seconds)
    }

    "should support Instant" in {
      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      XString(iso).as[Instant].shouldBe(instant)
    }

    "should support refined types" in {
      import refined.api.Refined
      import refined.numeric.Positive

      XString("-1000")
        .decode[Long Refined Positive]
        .leftValue
        .shouldBe("Predicate failed: (-1000 > 0).")
      XString("1000").as[Long Refined Positive].value.shouldBe(1000)
    }

    "should support generic products" in {
      import examples._

      XTag(
        "Foo",
        XTag("s", XString("hello")).asChild
      ).asChild
        .as[Foo]
        .shouldBe(
          Foo(
            "hello"
          )
        )
      XTag("Caz.type", XChildren(IList.empty)).asChild
        .as[Caz.type]
        .shouldBe(Caz)
      XString("Baz!").as[Baz.type].shouldBe(Baz)
      XTag("Faz", XTag("o", XString("hello")).asChild).asChild
        .as[Faz]
        .shouldBe(
          Faz(
            Some("hello")
          )
        )

      XString("").decode[Baz.type].leftValue.shouldBe("that's no Baz! ")

      // optional thing
      XTag("Faz", XTag("o", XChildren(IList.empty)).asChild).asChild
        .as[Faz]
        .shouldBe(Faz(None))

      // optional thing, key missing
      XTag("Faz", XChildren(IList.empty)).asChild
        .as[Faz]
        .shouldBe(Faz(None))

    }

    "should support generic coproducts" in {
      import examples._

      XTag(
        "SimpleTrait",
        IList(XAttr("typehint", XString("Foo"))),
        IList(XTag("s", XString("hello"))),
        Maybe.empty
      ).asChild.as[SimpleTrait].shouldBe(Foo("hello"))

      XTag(
        "SimpleTrait",
        IList(XAttr("typehint", XString("Caz"))),
        IList.empty,
        Maybe.empty
      ).asChild.as[SimpleTrait].shouldBe(Caz)

      XTag(
        "SimpleTrait",
        IList(XAttr("typehint", XString("Baz"))),
        IList.empty,
        Maybe.just(XString("Baz!"))
      ).asChild.as[SimpleTrait].shouldBe(Baz)

      XTag(
        "AbstractThing",
        IList(XAttr("typehint", XString("Wobble"))),
        IList(XTag("id", XString("fish"))),
        Maybe.empty
      ).asChild.as[AbstractThing].shouldBe(Wobble("fish"))

      XTag(
        "AbstractThing",
        IList(XAttr("typehint", XString("Wibble"))),
        IList.empty,
        Maybe.empty
      ).asChild.as[AbstractThing].shouldBe(Wibble)

      XTag(
        "CoproductInField",
        XTag(
          "abs",
          IList(XAttr("typehint", XString("Wibble"))),
          IList.empty,
          Maybe.empty
        ).asChild
      ).asChild.as[CoproductInField].shouldBe(CoproductInField(Wibble))
    }

    "should support generic recursive ADTs" in {
      import examples._

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
        .as[Recursive]
        .shouldBe(Recursive("hello", Some(Recursive("goodbye"))))
    }

    "should decode XmlAttribute fields" in {
      import examples._

      XTag(
        "MultiField",
        IList(XAttr("b", XString("goodbye"))),
        IList(XTag("a", XString("hello"))),
        Maybe.empty
      ).as[MultiField].shouldBe(MultiField("hello", "goodbye"))

      XTag("MultiOptyField", XTag("a", XString("hello")).asChild).asChild
        .as[MultiOptyField]
        .shouldBe(MultiOptyField("hello", None))

      XTag(
        "MultiFieldParent",
        IList(
          XAttr("typehint", XString("MultiField")),
          XAttr("b", XString("goodbye"))
        ),
        IList(XTag("a", XString("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(
          MultiField(
            "hello",
            "goodbye"
          )
        )

      XTag(
        "MultiFieldParent",
        IList(
          XAttr("typehint", XString("MultiOptyField")),
          XAttr("b", XString("goodbye"))
        ),
        IList(XTag("a", XString("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(
          MultiOptyField(
            "hello",
            Some("goodbye")
          )
        )

      XTag(
        "MultiFieldParent",
        IList(
          XAttr("typehint", XString("MultiOptyField"))
        ),
        IList(XTag("a", XString("hello"))),
        Maybe.empty
      ).asChild
        .as[MultiFieldParent]
        .shouldBe(MultiOptyField("hello", None))
    }

    "should support inlined semigroup fields" in {
      import examples._

      XTag(
        "Inliner",
        XChildren(
          IList(
            XTag(
              "Foo",
              XTag("s", XString("hello")).asChild
            ),
            XTag("nose", XString("goodbye"))
          )
        )
      ).asChild.as[Inliner].shouldBe(Inliner(Foo("hello"), "goodbye"))

      XTag(
        "InlinerSingle",
        XChildren(
          IList(
            XTag(
              "Foo",
              XTag("s", XString("hello")).asChild
            ),
            XTag(
              "Foo",
              XTag("s", XString("world")).asChild
            )
          )
        )
      ).asChild.as[InlinerSingle].shouldBe(InlinerSingle(Foo("helloworld")))

      XTag(
        "InlinerSingle",
        XChildren(
          IList(
            XTag(
              "Foo",
              XTag("a", XString("hello")).asChild
            ),
            XTag(
              "Foo",
              XTag("b", XString("world")).asChild
            )
          )
        )
      ).asChild
        .decode[InlinerSingle]
        .leftValue
        .shouldBe(
          """InlinerSingle -> wibble:
Foo -> expected one 's' with a body, got XChildren([XTag(Foo,[],[XTag(a,[],[],Just(XString(hello)))],Empty())])
Foo -> expected one 's' with a body, got XChildren([XTag(Foo,[],[XTag(b,[],[],Just(XString(world)))],Empty())])"""
        )
    }

    "should support inlined single fields" in {
      import examples._

      XTag(
        "AmbiguousCoproduct",
        XTag(
          "SimpleTrait",
          IList(XAttr("typehint", XString("Foo"))),
          IList(XTag("s", XString("hello"))),
          Maybe.empty
        ).asChild
      ).asChild
        .as[AmbiguousCoproduct]
        .shouldBe(
          AmbiguousCoproduct(Foo("hello"))
        )

      // sensible failure messages
      XTag(
        "AmbiguousCoproduct",
        XTag(
          "SimpleTrait",
          IList(XAttr("typehint", XString("Foo"))),
          IList(XTag("a", XString("hello"))),
          Maybe.empty
        ).asChild
      ).asChild
        .decode[AmbiguousCoproduct]
        .leftValue
        .shouldBe(
          """AmbiguousCoproduct -> foo:
SimpleTrait -> Foo -> expected one 's' with a body, got XChildren([XTag(SimpleTrait,[XAttr(typehint,XString(Foo))],[XTag(a,[],[],Just(XString(hello)))],Empty())])"""
        )

    }

    "should support inlined monoid lists" in {
      import examples._

      val xml2 = XTag(
        "Inliners",
        XChildren(
          IList(
            XTag("Foo", XTag("s", XString("hello")).asChild),
            XTag("Foo", XTag("s", XString("goodbye")).asChild)
          )
        )
      ).asChild

      xml2.as[Inliners].shouldBe(Inliners(List(Foo("hello"), Foo("goodbye"))))

      val xml3 = XTag(
        "Inliners",
        XChildren(
          IList(
            XTag("Foo", XTag("s", XString("hello")).asChild),
            XTag("Bar", XTag("s", XString("goodbye")).asChild),
            XTag("Foo", XTag("o", XString("invisible")).asChild),
            XTag("Baz", XTag("s", XString("oops")).asChild) // note different tag name!
          )
        )
      ).asChild

      xml3
        .as[Inliners]
        .shouldBe(Inliners(List(Foo("hello"), Foo("goodbye"), Foo("oops"))))

      val xml4 = XTag(
        "Inliners",
        XChildren(
          IList(
            XTag("Foo", XTag("a", XString("hello")).asChild),
            XTag("Bar", XTag("b", XString("goodbye")).asChild),
            XTag("Foo", XTag("c", XString("invisible")).asChild),
            XTag("Baz", XTag("d", XString("oops")).asChild) // note different tag name!
          )
        )
      ).asChild

      // never fails when there is a Monoid!
      xml4.as[Inliners].shouldBe(Inliners(Nil))
    }

    "should support inlined content" in {
      import examples._

      val xml1 = XTag(
        "Outliners",
        IList(XAttr("id", XString("hello"))),
        IList.empty,
        Maybe.just(XString("goodbye"))
      )

      xml1.as[Outliners].shouldBe(Outliners(Some("hello"), Some("goodbye")))

      val xml2 = XTag(
        "Outliners",
        IList(XAttr("id", XString("hello"))),
        IList.empty,
        Maybe.empty
      )

      xml2.as[Outliners].shouldBe(Outliners(Some("hello"), None))

      val xml3 = XTag(
        "Outliners",
        IList.empty,
        IList.empty,
        Maybe.just(XString("goodbye"))
      )

      xml3.as[Outliners].shouldBe(Outliners(None, Some("goodbye")))
    }

    "should support tagged coproduct disambiguation" in {
      import examples._

      XTag(
        "TaggyCoproduct",
        XTag(
          "TaggyA",
          XChildren(IList.empty)
        ).asChild
      ).asChild
        .as[TaggyCoproduct]
        .shouldBe(TaggyCoproduct(TaggyA()))

      XTag(
        "TaggyCoproduct",
        XTag(
          "TaggyB",
          XString("hello")
        ).asChild
      ).asChild
        .as[TaggyCoproduct]
        .shouldBe(TaggyCoproduct(TaggyB("hello")))

    }

  }

}
