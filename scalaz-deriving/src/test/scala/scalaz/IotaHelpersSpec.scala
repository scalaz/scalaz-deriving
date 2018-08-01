// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.Int

import iotaz._
import iotaz.TList._

import org.scalatest._
import org.scalatest.Matchers._

object IotaHelpersSpec {
  final case class Foo(s: String, ɩ: Int)

  final case class Goo[A](s: String, a: A)

  object Bar
  case object CBar

  sealed trait Traity1
  final case class Traity1A(s: String) extends Traity1
  case object Traity1B                 extends Traity1

  sealed trait Traity2
  final case class Traity2A[A](s: A) extends Traity2
  case object Traity2B               extends Traity2

  sealed trait ATree
  final case class Leaf(value: String)               extends ATree
  final case class Branch(left: ATree, right: ATree) extends ATree

  sealed trait GTree[A]
  final case class GLeaf[A](value: A)                          extends GTree[A]
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}

class IotaHelpersSpec extends FlatSpec {
  import IotaHelpersSpec._

  "ProdGen" should "support case classes" in {
    val foo = Foo("hello", 13)
    val gen = ProdGen.gen[Foo, String :: Int :: TNil]
    gen.from(gen.to(foo)).shouldBe(foo)
  }

  it should "support higher kinded case classes" in {
    val goo = Goo("hello", 13)

    def gen[A] =
      ProdGen.gen[Goo[Int], String :: Int :: TNil]
    val geni = gen[Int]

    geni.from(geni.to(goo)).shouldBe(goo)
  }

  it should "support objects" in {
    val gen = ProdGen.gen[Bar.type, TNil]
    gen.from(gen.to(Bar)).shouldBe(Bar)
  }

  it should "support case objects" in {
    val gen = ProdGen.gen[CBar.type, TNil]
    gen.from(gen.to(CBar)).shouldBe(CBar)
  }

  "CopGen" should "support sealed traits" in {
    val gen = CopGen.gen[Traity1, Traity1A :: Traity1B.type :: TNil]

    val a = Traity1A("hello")
    gen.from(gen.to(a)).shouldBe(a)

    val b = Traity1B
    gen.from(gen.to(b)).shouldBe(b)
  }

  it should "support sealed traits with generic parameters" in {
    def genG[A1] = CopGen.gen[Traity2, Traity2A[A1] :: Traity2B.type :: TNil]

    val gen = genG[String]

    val a = Traity2A("hello")
    gen.from(gen.to(a)).shouldBe(a)

    val b = Traity2B
    gen.from(gen.to(b)).shouldBe(b)

    val gena = CopGen.gen[Traity2, Traity2A[scala.Any] :: Traity2B.type :: TNil]

    gena.from(gena.to(a)).shouldBe(a)
    gena.from(gena.to(b)).shouldBe(b)

  }

  it should "support recursive ADTs" in {
    val gen = CopGen.gen[ATree, Leaf :: Branch :: TNil]

    val a = Leaf("foo")
    gen.from(gen.to(a)).shouldBe(a)

    val b = Branch(a, a)
    gen.from(gen.to(b)).shouldBe(b)
  }

  it should "support recursive GADTs" in {
    def gen[A] = CopGen.gen[GTree[A], GLeaf[A] :: GBranch[A] :: TNil]

    val gens = gen[String]

    val a = GLeaf("foo")
    gens.from(gens.to(a)).shouldBe(a)

    val b = GBranch(a, a)
    gens.from(gens.to(b)).shouldBe(b)
  }

}
