// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.examples

import scalaz.{ -\/, @@, \/- }
import scalaz.{ deriving, xderiving }

import xmlformat._

@xderiving(XEncoder, XDecoder)
final case class Optimal(thing: String) extends AnyVal

@deriving(XEncoder, XDecoder) sealed trait SimpleTrait
@deriving(XEncoder, XDecoder) final case class Foo(s: String)
    extends SimpleTrait
@deriving(XEncoder, XDecoder) final case class Bar() extends SimpleTrait
@deriving(XEncoder, XDecoder) case object Caz        extends SimpleTrait
case object Baz extends SimpleTrait {
  // user-provided override on the companion
  implicit val e: XEncoder[Baz.type] = XEncoder[String].contramap { _ =>
    "Baz!"
  }
  implicit val d: XDecoder[Baz.type] = XDecoder[String].emap {
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
  implicit val e: XEncoder[Foo] = XEncoder[String].contramap(_.s)
  implicit val d: XDecoder[Foo] = XDecoder[String].map(Foo(_))

  implicit val ste: XEncoder[SimpleTrait] = DerivedXEncoder.gen
  implicit val std: XDecoder[SimpleTrait] = DerivedXDecoder.gen
}

@deriving(XEncoder, XDecoder) sealed abstract class AbstractThing(
  val id: String
)
@deriving(XEncoder, XDecoder) case object Wibble extends AbstractThing("wibble")
@deriving(XEncoder, XDecoder) final case class Wobble(override val id: String)
    extends AbstractThing(id)

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
final case class Inliner(Foo: Foo @@ XInlinedField, nose: String)
@deriving(XEncoder, XDecoder)
final case class InlinerSingle(Foo: Foo @@ XInlinedField)
@deriving(XEncoder, XDecoder)
final case class Inliners(Foo: List[Foo] @@ XInlinedList @@ XInlinedField)
@deriving(XEncoder, XDecoder)
final case class Outliners(id: Option[String] @@ XAttribute,
                           body: Option[String] @@ XInlinedContent)

// decoding will fail. These shapes require a hand-written decoder, see notes on
// DerivedXDecoder.flatChildren
@deriving(XEncoder, XDecoder)
final case class NestedSingle(Foo: Foo)
