// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.lang.String
import java.math.{ BigDecimal => BD }

import scala.Predef.ArrowAssoc
import scala.collection.immutable._
import scala.concurrent.duration._
import scala.xml._

import org.scalatest._
import org.scalatest.Matchers._

class EncoderTests extends FreeSpec {
  import Encoder.ops._

  implicit class Helper[T: Encoder](t: T) {
    def xmlString: String = t.toXml.toString
  }

  "XML Encoder" - {
    "should identity map xml literals" in {
      val foo = <foo>{ "hello" }</foo>

      foo.toXml should be theSameInstanceAs foo
    }

    "should identity map existing Node" in {
      val foo: NodeSeq = <foo>{ "hello" }</foo>

      foo.toXml should be theSameInstanceAs foo
    }

    "should support Boolean" in {
      true.toXml shouldBe new Atom("true")
      false.toXml shouldBe new Atom("false")
    }

    "should support java BigDecimal" in {
      (new BD("1.0")).toXml shouldBe new Atom("1.0")
    }

    "should support integers" in {
      val expected = new Atom("13")

      13.toShort.toXml shouldBe expected
      13.toInt.toXml shouldBe expected
      13.toLong.toXml shouldBe expected

      13.toLong.xmlString shouldBe "13"
    }

    "should support floating point numbers" in {
      val expected = new Atom("0.1")

      0.1.toFloat.toXml shouldBe expected
      0.1.toDouble.toXml shouldBe expected

      0.1.toDouble.xmlString shouldBe "0.1"
    }

    "should support single characters" in {
      'c'.toXml shouldBe new Text("c")
      'c'.xmlString shouldBe "c"
    }

    "should support Strings" in {
      "<wibble><wobble".toXml shouldBe new Text("<wibble><wobble")
      "<wibble><wobble".xmlString shouldBe "&lt;wibble&gt;&lt;wobble"
    }

    "should support Symbols" in {
      'foo.xmlString shouldBe "foo"
    }

    "should special-case Option" in {
      (Some("hello"): Option[String]).xmlString shouldBe "hello"

      (None: Option[String]).toXml shouldBe Group(Nil)
      (None: Option[String]).xmlString shouldBe ""
    }

    "should special-case Either" in {
      (Left("hello"): Either[String, Int]).xmlString shouldBe "<Left>hello</Left>"
      (Right(13): Either[String, Int]).xmlString shouldBe "<Right>13</Right>"
    }

    "should support Traversables" in {
      Seq(1, 2, 3).xmlString shouldBe "<value>1</value><value>2</value><value>3</value>"
      Set(1, 2, 3).xmlString shouldBe "<value>1</value><value>2</value><value>3</value>"

      (ListSet(1, 2, 3).xmlString should
        (equal("<value>1</value><value>2</value><value>3</value>")
          or equal("<value>3</value><value>2</value><value>1</value>")))

      List(1, 2, 3).xmlString shouldBe "<value>1</value><value>2</value><value>3</value>"
    }

    "should special case Map[Thing, OtherThing]" in {
      Map(1 -> "a", 2 -> "b", 3 -> "c").xmlString shouldBe "<entry><key>1</key><value>a</value></entry><entry><key>2</key><value>b</value></entry><entry><key>3</key><value>c</value></entry>"
    }

    "should support FiniteDuration" in {
      10.seconds.xmlString shouldBe "10000"
    }

    "should special-case AnyVal" in {
      import examples._

      Optimal("foo").xmlString shouldBe "foo"
    }

    "should support generic products" in {
      import examples._

      Foo("hello").xmlString shouldBe "<s>hello</s>"
      Caz.xmlString shouldBe ""
      Baz.xmlString shouldBe "Baz!"
      Faz(Some("hello")).xmlString shouldBe "<o>hello</o>"
    }

    "should support generic coproducts" in {
      import examples._

      (Foo("hello"): SimpleTrait).xmlString shouldBe "<Foo><s>hello</s></Foo>"
      (Caz: SimpleTrait).xmlString shouldBe "<Caz/>"
      (Baz: SimpleTrait).xmlString shouldBe "<Baz>Baz!</Baz>"

      (Wobble("fish"): AbstractThing).xmlString shouldBe ("<Wobble><id>fish</id></Wobble>")

      (Wibble: AbstractThing).xmlString shouldBe "<Wibble/>"
    }

    "should support generic recursive ADTs" in {
      import examples._

      val rec = Recursive("hello", Some(Recursive("goodbye")))
      rec.xmlString shouldBe "<h>hello</h><t><h>goodbye</h><t/></t>"
    }

    "should respect user orphan overrides" ignore {
      import examples._
      import examples.orphans._

      Foo("hello").xmlString shouldBe "hello"
      (Foo("hello"): SimpleTrait).xmlString shouldBe "<Foo>hello</Foo>"
    }
  }

}
