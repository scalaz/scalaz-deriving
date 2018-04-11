// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package scalaxml

import org.scalatest.{ Tag => _, _ }
import org.scalactic.source.Position
import org.scalatest.Matchers._

import scalaz._

class DecoderTests extends FreeSpec {
  import XTestUtils._
  import Decoder.ops._

  implicit class XmlHelper(x: xml.NodeSeq) {
    def as[T: XDecoder](implicit P: Position): T = x.decode[T].rightValue
  }

  implicit class StringHelper(txt: String) {
    def as[T: XDecoder](implicit P: Position): T =
      parseChild.decode[T].rightValue
    def failsAs[T: XDecoder](implicit P: Position): String =
      parseChild.decode[T].leftValue

    def parseChild: xml.NodeSeq =
      Decoder.secureLoadString(s"""<value>$txt</value>""").child match {
        case Seq(only) => only
        case many      => xml.Group(many.toList)
      }

    def parsedAs[T: Decoder](implicit P: Position): T =
      Decoder[T].fromXml(parseChild).rightValue
  }

  "XNode Decoder" - {

    "should support XAtom" in {
      xml.Text("wibble").as[XNode].shouldBe(XText("wibble"))

      "&lt;wibble&gt;".parsedAs[XNode].shouldBe(XText("<wibble>"))

      xml.PCData("wibble").as[XNode].shouldBe(XCdata("wibble"))

      // magical conversions inside scala.xml...
      "<![CDATA[<wibble>]]>".parsedAs[XNode].shouldBe(XText("<wibble>"))
    }

    "should fail on unparsed input" in {
      Decoder[XNode]
        .fromXml(xml.Unparsed("foo"))
        .leftValue
        .shouldBe("encountered unparsed xml: foo")
    }

    "should support groups" in {
      xml
        .Group(
          Seq[xml.Node](
            xml.Elem(
              null /* scalafix:ok */,
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
              null /* scalafix:ok */,
              "bar",
              xml.Null,
              xml.TopScope,
              true,
              xml.Text("wobble")
            )
          )
        )
        .as[XNode]
        .shouldBe(
          XChildren(
            IList(
              XTag(
                XAtom("foo"),
                IList(XAttr(XAtom("bar"), XText("<wobble>"))),
                IList.empty,
                Maybe.just(XText("wibble"))
              ),
              XTag(
                XAtom("bar"),
                XText("wobble")
              )
            )
          )
        )

    }

    "should support elements" in {
      "<foo>wibble</foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag(
            XAtom("foo"),
            XText("wibble")
          )
        )

      "<foo bar='wobble'>wibble</foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag(
            XAtom("foo"),
            IList(XAttr(XAtom("bar"), XText("wobble"))),
            IList.empty,
            Maybe.just(XText("wibble"))
          )
        )

      "<foo bar='BAR' baz='BAZ'><wobble/><fish/></foo>"
        .parsedAs[XNode]
        .shouldBe(
          XTag(
            XAtom("foo"),
            IList(
              XAttr(XAtom("baz"), XText("BAZ")),
              XAttr(XAtom("bar"), XText("BAR"))
            ),
            IList(
              XTag(XAtom("wobble"), XChildren(IList.empty)),
              XTag(XAtom("fish"), XChildren(IList.empty))
            ),
            Maybe.empty
          )
        )
    }

    "should support nested CDATA" in {
      val raw = "<Foo><![CDATA[%s]]></Foo>"

      val cdata = xml.PCData(raw)

      // the PCData constructor escapes nested CDATA...
      cdata.data.shouldBe("<Foo><![CDATA[%s]]]]><![CDATA[></Foo>")

      // but we recover the original
      cdata.as[XNode].shouldBe(XCdata("<Foo><![CDATA[%s]]></Foo>"))
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

    "should special-case Option" in {
      "hello".as[Option[String]].shouldBe(Some("hello"))

      xml.Group(Nil).as[Option[String]].shouldBe(None)

      // this was not designed to work this way, but it seems ok...
      "".as[Option[String]].shouldBe(None)
    }

    "should special-case Either" in {
      "hello".as[Either[Int, String]].shouldBe(Right("hello"))
      "13".as[Either[Int, String]].shouldBe(Left(13))
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

    "should support generic products" in {
      import examples._

      "".failsAs[Foo].shouldBe("Foo -> missing tag 's'")

      "".failsAs[MultiField].shouldBe("MultiField -> missing tag 'a'")

      "<s>hello</s>".as[Foo].shouldBe(Foo("hello"))
      "".as[Caz.type].shouldBe(Caz)
      "Baz!".as[Baz.type].shouldBe(Baz)

      "flibble".failsAs[Baz.type].shouldBe("that's no Baz! flibble")

      "<o>hello</o>".as[Faz].shouldBe(Faz(Some("hello")))
    }

    "should support generic coproducts" in {
      import examples._

      "<meh/>"
        .failsAs[SimpleTrait]
        .shouldBe(
          "SimpleTrait -> no valid typehint in 'XTag(XAtom(meh),[],[],Empty())'"
        )

      "<Foo><s>hello</s></Foo>".as[SimpleTrait].shouldBe(Foo("hello"))
      "<Caz/>".as[SimpleTrait].shouldBe(Caz)
      "<Baz>Baz!</Baz>".as[SimpleTrait].shouldBe(Baz)

      "<Wobble><id>fish</id></Wobble>"
        .as[AbstractThing]
        .shouldBe(Wobble("fish"))

      "<Wibble/>".as[AbstractThing].shouldBe(Wibble)
    }

    "should support generic recursive ADTs" in {
      import examples._

      val rec = Recursive("hello", Some(Recursive("goodbye")))
      "<h>hello</h><t><h>goodbye</h><t/></t>".as[Recursive].shouldBe(rec)
    }

    "should decode XmlAttribute fields" in {
      import examples._

      """<MultiField b="goodbye"><a>hello</a></MultiField>"""
        .as[MultiFieldParent]
        .shouldBe(MultiField("hello", Tag("goodbye")))
    }

  }
}
