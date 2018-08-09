// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import org.scalatest._
import org.scalactic.source.Position
import org.scalatest.Matchers._
import eu.timepit.refined

import scalaz._

class DecoderTests extends FreeSpec {
  import XTestUtils._
  import Decoder.xnode

  implicit class XmlHelper(x: xml.NodeSeq) {
    def as[T: XNodeDecoder](implicit P: Position): T =
      XNodeDecoder[T].fromXml(asX).rightValue
    def asX: XNode = xnode.fromScalaXml(x).rightValue
  }

  implicit class StringHelper(txt: String) {
    def as[T: XNodeDecoder](implicit P: Position): T = parseChild.as[T]
    def failsAs[T: XNodeDecoder](implicit P: Position): String =
      XNodeDecoder[T].fromXml(parseChild.asX).leftValue

    def parseChild: xml.NodeSeq =
      Decoder
        .secureLoadString(s"""<value>$txt</value>""")
        .rightValue
        .child match {
        case Seq(only) => only
        case many      => xml.Group(many.toList)
      }

    def parsedAs[T: Decoder](implicit P: Position): T =
      Decoder[T].fromScalaXml(parseChild).rightValue
  }

  "XNode Decoder" - {

    "should support XString" in {
      xml.Text("wibble").asX.shouldBe(XString("wibble"))

      "&lt;wibble&gt;".parsedAs[XNode].shouldBe(XString("<wibble>"))

      xml.PCData("wibble").asX.shouldBe(XString("wibble"))

      // magical conversions inside scala.xml...
      "<![CDATA[<wibble>]]>".parsedAs[XNode].shouldBe(XString("<wibble>"))
    }

    "should fail on unparsed input" in {
      Decoder[XNode]
        .fromScalaXml(xml.Unparsed("foo"))
        .leftValue
        .shouldBe("encountered unparsed xml: foo")
    }

    "should support groups" in {
      xml
        .Group(
          Seq[xml.Node](
            xml.Elem(
              null, // scalafix:ok
              "foo",
              new xml.UnprefixedAttribute(
                "bar",
                xml.Text("<wobble>"),
                xml.Null
              ),
              xml.TopScope,
              true,
              xml.Text("wibble")
            ),
            xml.Elem(
              null, // scalafix:ok
              "bar",
              xml.Null,
              xml.TopScope,
              true,
              xml.Text("wobble")
            )
          )
        )
        .asX
        .shouldBe(
          XChildren(
            IList(
              XTag(
                "foo",
                IList(XAttr("bar", XString("<wobble>"))),
                IList.empty,
                Maybe.just(XString("wibble"))
              ),
              XTag("bar", XString("wobble"))
            )
          )
        )

    }

    "should support elements" in {
      "<foo>wibble</foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag("foo", XString("wibble")).asChild
        )

      "<foo bar='wobble'>wibble</foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag(
            "foo",
            IList(XAttr("bar", XString("wobble"))),
            IList.empty,
            Maybe.just(XString("wibble"))
          ).asChild
        )

      "<foo bar='BAR' baz='BAZ'><wobble/><fish/></foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag(
            "foo",
            IList(
              XAttr("baz", XString("BAZ")),
              XAttr("bar", XString("BAR"))
            ),
            IList(
              XTag("wobble", XChildren(IList.empty)),
              XTag("fish", XChildren(IList.empty))
            ),
            Maybe.empty
          ).asChild
        )
    }

    // fixed in https://github.com/scala/scala-xml/commit/bcde63
    "should support nested CDATA" ignore {
      val raw = "<Foo><![CDATA[%s]]></Foo>"

      val cdata = xml.PCData(raw)

      // the PCData constructor escapes nested CDATA...
      cdata.data.shouldBe("<Foo><![CDATA[%s]]]]><![CDATA[></Foo>")

      // but we recover the original
      cdata.asX.shouldBe(XString("<Foo><![CDATA[%s]]></Foo>"))
    }

  }

  "XML Decoder" - {
    "should support Boolean" in {
      "true".as[Boolean].shouldBe(true)
      "false".as[Boolean].shouldBe(false)
    }

    "should support integers" in {
      "13".as[Short].shouldBe(13.toShort)
      "13".as[Int].shouldBe(13.toInt)
      "13".as[Long].shouldBe(13.toLong)
    }

    "should support floating point numbers" in {
      "0.1".as[Float].shouldBe(0.1.toFloat)
      "0.1".as[Double].shouldBe(0.1.toDouble)
    }

    "should support single characters" in {
      "c".as[Char].shouldBe('c')

      "ca".failsAs[Char].shouldBe("text too long: ca")
    }

    "should support Strings" in {
      "&lt;wibble&gt;&lt;wobble".as[String].shouldBe("<wibble><wobble")
    }

    "should support Symbols" in {
      "foo".as[Symbol].shouldEqual('foo)
    }

    "should special-case Either" in {
      "hello".as[Either[Int, String]].shouldBe(Right("hello"))
      "13"
        .failsAs[Either[Int, String]]
        .shouldBe("unable to disambiguate 'XString(13)'")
    }

    "should support Traversables" in {

      "<value>1</value><value>2</value><value>3</value>"
        .as[List[Int]]
        .shouldBe(List(1, 2, 3))
      "<value>1</value><value>2</value><value>3</value>"
        .as[Seq[Int]]
        .shouldBe(Seq(1, 2, 3))
      "<value>1</value><value>2</value><value>3</value>"
        .as[Set[Int]]
        .shouldBe(Set(1, 2, 3))
      "<value>1</value>".as[Seq[Int]].shouldBe(Seq(1))

      "".as[List[Int]].shouldBe(Nil)
    }

    "should special case Map[Thing, OtherThing]" in {
      "<entry><key>1</key><value>a</value></entry><entry><key>2</key><value>b</value></entry><entry><key>3</key><value>c</value></entry>"
        .as[Map[Int, String]]
        .shouldBe(Map(1 -> "a", 2 -> "b", 3 -> "c"))
    }

    "should support NonEmptyList" in {
      "<value>1</value><value>2</value><value>3</value>"
        .as[NonEmptyList[Int]]
        .shouldBe(
          NonEmptyList(1, 2, 3)
        )

      "".failsAs[NonEmptyList[Int]].shouldBe("list was empty")
    }

    "should support FiniteDuration" in {
      import scala.concurrent.duration._

      "10000".as[FiniteDuration].shouldBe(10.seconds)
    }

    "should support Instant" in {
      import java.time.Instant

      val iso     = "2013-05-30T23:38:23.085Z"
      val instant = Instant.parse(iso)
      iso.as[Instant].shouldBe(instant)
    }

    "should support Refined types" in {
      import refined.api.Refined
      import refined.numeric.Positive

      "1000".as[Long Refined Positive].value.shouldBe(1000)

      "-1000"
        .failsAs[Long Refined Positive]
        .shouldBe("Predicate failed: (-1000 > 0).")
    }

    "should support generic products" in {
      import examples._

      "".failsAs[Foo]
        .shouldBe("Foo -> expected one tag, got XChildren([])")

      "".failsAs[MultiField]
        .shouldBe("MultiField -> expected one tag, got XChildren([])")

      "<Foo><s>hello</s></Foo>".as[Foo].shouldBe(Foo("hello"))
      "<Caz.type/>".as[Caz.type].shouldBe(Caz)
      "Baz!".as[Baz.type].shouldBe(Baz)

      "flibble".failsAs[Baz.type].shouldBe("that's no Baz! flibble")

      "<Faz><o>hello</o></Faz>".as[Faz].shouldBe(Faz(Some("hello")))
    }

    "should support generic coproducts" in {
      import examples._

      "<meh/>"
        .failsAs[SimpleTrait]
        .shouldBe(
          "SimpleTrait -> expected a valid typehint, got XChildren([XTag(meh,[],[],Empty())])"
        )

      """<SimpleTrait typehint="Foo"><s>hello</s></SimpleTrait>"""
        .as[SimpleTrait]
        .shouldBe(Foo("hello"))
      """<SimpleTrait typehint="Caz"/>""".as[SimpleTrait].shouldBe(Caz)
      """<SimpleTrait typehint="Baz">Baz!</SimpleTrait>"""
        .as[SimpleTrait]
        .shouldBe(Baz)

      """<AbstractThing typehint="Wobble"><id>fish</id></AbstractThing>"""
        .as[AbstractThing]
        .shouldBe(Wobble("fish"))

      """<AbstractThing typehint="Wibble"/>"""
        .as[AbstractThing]
        .shouldBe(Wibble)
    }

    "should support generic recursive ADTs" in {
      import examples._

      val rec = Recursive("hello", Some(Recursive("goodbye")))
      "<Recursive><h>hello</h><t><h>goodbye</h></t></Recursive>"
        .as[Recursive]
        .shouldBe(rec)
    }

    "should decode XmlAttribute fields" in {
      import examples._

      """<MultiFieldParent typehint="MultiField" b="goodbye"><a>hello</a></MultiFieldParent>"""
        .as[MultiFieldParent]
        .shouldBe(MultiField("hello", "goodbye"))
    }

  }
}
