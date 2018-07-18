// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._
import org.scalactic.source.Position
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import examples.anyvals._
import examples.adt._

import Scalaz._

import examples._
import examples.NamePacked._
import examples.NamePacker.ops._
import examples.NameUnpacker.ops._

class LabelledDecoderSpec
    extends FlatSpec
    with NonImplicitAssertions
    with GeneratorDrivenPropertyChecks {

  implicit class Helper(n: NamePacked) {
    def shouldDecodeAs[A: NameUnpacker](a: A)(implicit P: Position): Assertion =
      n.decode[A].shouldBe(a.right)

    def shouldFailAs[A: NameUnpacker](
      err: String
    )(implicit P: Position): Assertion =
      n.decode[A].leftMap(_.toString).shouldBe(err.left)
  }

  "anyvals" should "behave as expected" in {
    Characters("greetings").shouldDecodeAs(Thing("greetings"))

    Rational(0).shouldFailAs[Thing]("expected Characters, got Rational(long=0)")

  }

  val bar: Foo = Bar("hello")
  val baz: Foo = Baz
  val faz: Foo = Faz(true, 1)

  "products" should "behave as expected" in {
    Product(IList(("s", Characters("hello")))).shouldDecodeAs(Bar("hello"))

    Product(IList.empty).shouldDecodeAs(Baz)

    Product(IList(("b", Rational(1)), ("i", Rational(1))))
      .shouldDecodeAs(Faz(true, 1))

    Product(IList(("x", Rational(1)), ("y", Rational(1))))
      .shouldFailAs[Faz]("""expected field "b", got ["x","y"]""")

  }

  "coproducts" should "behave as expected" in {

    Product(IList(("typehint", Characters("Bar")), ("s", Characters("hello"))))
      .shouldDecodeAs[Foo](Bar("hello"))

    Product(IList(("b", Rational(1)), ("i", Rational(1))))
      .shouldFailAs[Foo]("""expected "typehint", got ["b","i"]""")

    // better errors here would be nice
    // https://gitlab.com/fommil/scalaz-deriving/issues/93
    Product(
      IList(("typehint", Characters("Wibble")), ("s", Characters("hello")))
    ).shouldFailAs[Foo]("""a valid "Wibble"""")
  }

  "recursive ADTs" should "round trip encoding and decoding" in {
    import examples.recadt._

    forAll { (t: ATree) =>
      t.encode.decode[ATree].shouldBe(t.right)
    }
  }

  "recursive GADTs" should "round trip encoding and decoding" in {
    import examples.recgadt._

    forAll { (t: GTree[String]) =>
      t.encode.decode[GTree[String]].shouldBe(t.right)
    }
  }
}
