// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.examples

import scalaz.{ -\/, @@, \/-, Semigroup }
import scalaz.std.string._
import scalaz.std.list._
import scalaz.syntax.invariantFunctor._
import scalaz.{ deriving, xderiving }

import xmlformat._

@xderiving(XStrEncoder, XStrDecoder)
final case class Optimal(thing: String) extends AnyVal

@deriving(XEncoder, XDecoder) sealed trait SimpleTrait

@xderiving(Semigroup) // for inlining
@deriving(XEncoder, XDecoder)
final case class Foo(s: String) extends SimpleTrait

@deriving(XEncoder, XDecoder) final case class Bar() extends SimpleTrait
@deriving(XEncoder, XDecoder) case object Caz        extends SimpleTrait
case object Baz extends SimpleTrait {
  // user-provided override on the companion
  implicit val e: XStrEncoder[Baz.type] = XStrEncoder[String].contramap { _ =>
    "Baz!"
  }
  implicit val d: XStrDecoder[Baz.type] = XStrDecoder[String].emap {
    case "Baz!" => \/-(Baz)
    case other  => -\/(s"that's no Baz! $other")
  }
}
@deriving(XEncoder, XDecoder) final case class Faz(o: Option[String])
    extends SimpleTrait

@deriving(XEncoder, XDecoder) final case class Recursive(h: String,
                                                         t: Option[Recursive] =
                                                           None)

object orphans {
  implicit val e: XStrEncoder[Foo] = XStrEncoder[String].contramap(_.s)
  implicit val d: XStrDecoder[Foo] = XStrDecoder[String].map(Foo(_))

  implicit val ste: XEncoder[SimpleTrait] = generic.DerivedXEncoder.gen
  implicit val std: XDecoder[SimpleTrait] = generic.DerivedXDecoder.gen
}

@deriving(XEncoder, XDecoder) sealed abstract class AbstractThing(
  val id: String
)
@deriving(XEncoder, XDecoder) case object Wibble extends AbstractThing("wibble")
@deriving(XEncoder, XDecoder) final case class Wobble(override val id: String)
    extends AbstractThing(id)

@deriving(XEncoder, XDecoder)
final case class CoproductInField(abs: AbstractThing)

@deriving(XEncoder, XDecoder)
final case class AmbiguousCoproduct(foo: SimpleTrait @@ XInlined)

@deriving(XEncoder, XDecoder) sealed abstract class MultiFieldParent
@deriving(XEncoder, XDecoder) final case class MultiField(
  a: String,
  b: String @@ XAttribute
) extends MultiFieldParent

@deriving(XEncoder, XDecoder) final case class MultiOptyField(
  a: String,
  b: Option[String] @@ XAttribute
) extends MultiFieldParent

@deriving(XEncoder, XDecoder)
final case class Inliner(wibble: Foo @@ XInlined, nose: String)
@deriving(XEncoder, XDecoder)
final case class InlinerSingle(wibble: Foo @@ XInlined)
@deriving(XEncoder, XDecoder)
final case class Inliners(foos: List[Foo] @@ XInlined)
@deriving(XEncoder, XDecoder)
final case class Outliners(id: Option[String] @@ XAttribute,
                           body: Option[String] @@ XInlined)

sealed abstract class TaggyNames
object TaggyNames {
  implicit val xencoder: XEncoder[TaggyNames] = generic.DerivedXEncoderTag.gen
  implicit val xdecoder: XDecoder[TaggyNames] = generic.DerivedXDecoderTag.gen
}

@deriving(XEncoder, XDecoder)
final case class TaggyA() extends TaggyNames
@xderiving(XStrEncoder, XStrDecoder)
final case class TaggyB(body: String) extends TaggyNames

@deriving(XEncoder, XDecoder)
final case class TaggyCoproduct(foo: TaggyNames @@ XInlined)
