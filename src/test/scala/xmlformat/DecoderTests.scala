// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package xmlformat

import java.lang.String
import java.math.{ BigDecimal => BD }

import scala._
import scala.Predef.ArrowAssoc
import scala.collection.immutable.{ ListSet, Map, Set }
import scala.concurrent.duration._
import scala.xml._

import org.scalactic.source.Position
import org.scalatest._
import org.scalatest.EitherValues._
import org.scalatest.Matchers._
import scalaz._

class DencoderTests extends FreeSpec {
  import Decoder.ops._
  import DecoderUtils._

  implicit class StringHelper(xml: String) {
    def as[T: Decoder](implicit P: Position): T =
      XML.loadString(xml).decode[T].right.value

    def parseChild: NodeSeq = {
      val doc = s"""<value>$xml</value>"""
      XML.loadString(doc).children
    }

    def parsedAs[T: Decoder](implicit P: Position): T =
      Decoder[T].fromXml(xml.parseChild).right.value

    def failsAs[T: Decoder](implicit P: Position): NonEmptyList[String] =
      Decoder[T].fromXml(xml.parseChild).left.value

  }
  implicit class XmlHelper(xml: NodeSeq) {
    def as[T: Decoder](implicit P: Position): T =
      Decoder[T].fromXml(xml).right.value
  }

  "XML Decoder" - {
    "should identity map xml literals" in {
      val foo: NodeSeq = <foo>{ "hello" }</foo>

      foo.as[NodeSeq] should be theSameInstanceAs foo
    }

    "should support Boolean" in {
      new Atom("true").as[Boolean] shouldBe true
      new Atom("false").as[Boolean] shouldBe false

      new Text("true").as[Boolean] shouldBe true
      new Text("false").as[Boolean] shouldBe false

      new Unparsed("true").as[Boolean] shouldBe true
      new Unparsed("false").as[Boolean] shouldBe false

      "true".parsedAs[Boolean] shouldBe true
      "false".parsedAs[Boolean] shouldBe false

    }

    "should support java BigDecimal" in {
      "1.0".parsedAs[BD] shouldBe new BD("1.0")
    }

    "should support integers" in {
      "13".parsedAs[Short] shouldBe 13.toShort
      "13".parsedAs[Int] shouldBe 13.toInt
      "13".parsedAs[Long] shouldBe 13.toLong
    }

    "should support floating point numbers" in {
      "0.1".parsedAs[Float] shouldBe 0.1.toFloat
      "0.1".parsedAs[Double] shouldBe 0.1.toDouble
    }

    "should support single characters" in {
      "ca".failsAs[Char] shouldBe failure("text too long: ca")

      "c".parsedAs[Char] shouldBe 'c'
    }

    "should support Strings" in {
      "&lt;wibble&gt;&lt;wobble".parsedAs[String] shouldBe "<wibble><wobble"
    }

    "should support Symbols" in {
      "foo".parsedAs[Symbol] shouldEqual 'foo
    }

    "should special-case Option" in {
      "hello".parsedAs[Option[String]] shouldBe Some("hello")
      Group(Nil).as[Option[String]] shouldBe None
      "".parsedAs[Option[String]] shouldBe None
    }

    "should special-case Either" in {
      "<Left>hello</Left>".parsedAs[Either[String, Int]] shouldBe Left("hello")
      "<Right>13</Right>".parsedAs[Either[String, Int]] shouldBe Right(13)
    }

    "should support Traversables" in {
      "<value>1</value><value>2</value><value>3</value>"
        .parsedAs[List[Int]] shouldBe List(1, 2, 3)
      "<value>1</value><value>2</value><value>3</value>"
        .parsedAs[Seq[Int]] shouldBe Seq(1, 2, 3)
      "<value>1</value><value>2</value><value>3</value>"
        .parsedAs[Set[Int]] shouldBe Set(1, 2, 3)
      "<value>3</value><value>2</value><value>1</value>"
        .parsedAs[ListSet[Int]] shouldBe ListSet(1, 2, 3)

      "<value>1</value>".parsedAs[Seq[Int]] shouldBe Seq(1)
    }

    "should special case Map[Thing, OtherThing]" in {
      "<entry><key>1</key><value>a</value></entry><entry><key>2</key><value>b</value></entry><entry><key>3</key><value>c</value></entry>"
        .parsedAs[Map[Int, String]] shouldBe Map(1 -> "a", 2 -> "b", 3 -> "c")
    }

    "should support NonEmptyList" in {
      "<value>1</value><value>2</value><value>3</value>"
        .parsedAs[NonEmptyList[Int]] shouldBe NonEmptyList(1, 2, 3)

      "".failsAs[NonEmptyList[Int]] shouldBe failure("list was empty")
    }

    "should support FiniteDuration" in {
      "10000".parsedAs[FiniteDuration] shouldBe 10.seconds
    }

    "should special-case AnyVal" in {
      import examples._

      "foo".parsedAs[Optimal] shouldBe Optimal("foo")
    }

    "should support generic products" in {
      import examples._

      "".failsAs[Foo] shouldBe NonEmptyList(
        "missing element s",
        "when decoding xmlformat.examples.Foo"
      )
      "".failsAs[MultiField] shouldBe NonEmptyList(
        "missing element a",
        "missing element b",
        "when decoding xmlformat.examples.MultiField"
      )

      "<s>hello</s>".parsedAs[Foo] shouldBe Foo("hello")
      "".parsedAs[Caz.type] shouldBe Caz
      "Baz!".parsedAs[Baz.type] shouldBe Baz
      "".failsAs[Baz.type] shouldBe a[NonEmptyList[_]]

      "<o>hello</o>".parsedAs[Faz] shouldBe Faz(Some("hello"))
    }

    "should support generic coproducts" in {
      import examples._

      "".failsAs[SimpleTrait] shouldBe NonEmptyList(
        "no valid typehint in ''",
        "when decoding xmlformat.examples.SimpleTrait"
      )

      "<Foo><s>hello</s></Foo>".parsedAs[SimpleTrait] shouldBe Foo("hello")
      "<Caz/>".parsedAs[SimpleTrait] shouldBe Caz
      "<Baz>Baz!</Baz>".parsedAs[SimpleTrait] shouldBe Baz

      "<Wobble><id>fish</id></Wobble>"
        .parsedAs[AbstractThing] shouldBe Wobble("fish")

      "<Wibble/>".parsedAs[AbstractThing] shouldBe Wibble
    }

    "should support generic recursive ADTs" in {
      import examples._

      val rec = Recursive("hello", Some(Recursive("goodbye")))
      "<h>hello</h><t><h>goodbye</h><t/></t>".parsedAs[Recursive] shouldBe rec
    }

    "should respect user orphan overrides" ignore {
      import examples._
      import examples.orphans._

      "<Foo>hello</Foo>".parsedAs[SimpleTrait] shouldBe Foo("hello")
    }

    "should support Unparsed" in {
      "<Foo>hello</Foo>".as[Unparsed] shouldBe Unparsed("<Foo>hello</Foo>")
    }

    "should support PCData" in {
      "<Foo>hello</Foo>".parsedAs[PCData] shouldBe PCData("<Foo>hello</Foo>")

      // nested XML is reparsed
      "<Foo><![CDATA[%s]]></Foo>"
        .parsedAs[PCData]
        .text shouldBe "<Foo>%s</Foo>"

      val top =
        """<?xml version="1.0" encoding="UTF-8"?>
           <GRAND
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="vast.xsd" version="2.0">
             <Foo><![CDATA[%s]]></Foo>
           </GRAND>"""

      // https://github.com/scala/scala-xml/issues/160
      intercept[org.xml.sax.SAXParseException] {
        PCData(top).toString.parsedAs[PCData]
      }

    }

  }

}
