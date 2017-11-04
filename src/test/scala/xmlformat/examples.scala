// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.examples

import java.lang.String

import scala.{ AnyVal, Left, None, Option, Right, StringContext }

import xmlformat._
import xmlformat.DecoderUtils._

import stalactite._

import scalaz._
import Scalaz._

@deriving(Encoder, Decoder)
final case class Optimal(thing: String) extends AnyVal

@deriving(Encoder, Decoder) sealed trait SimpleTrait
@deriving(Encoder, Decoder) final case class Foo(s: String) extends SimpleTrait
@deriving(Encoder, Decoder) final case class Bar()          extends SimpleTrait
@deriving(Encoder, Decoder) case object Caz                 extends SimpleTrait
case object Baz extends SimpleTrait {
  // user-provided override on the companion
  implicit val e: Encoder[Baz.type] = Encoder[String].contramap { _ =>
    "Baz!"
  }
  implicit val d: Decoder[Baz.type] = Decoder[String].andThen {
    case "Baz!" => Right(Baz)
    case other  => Left(failure(s"that's no Baz! $other"))
  }
}
@deriving(Encoder, Decoder) final case class Faz(o: Option[String])
    extends SimpleTrait

@deriving(Encoder, Decoder) final case class Recursive(h: String,
                                                       t: Option[Recursive] =
                                                         None)

object orphans {
  implicit val e: Encoder[Foo] = Encoder[String].contramap(_.s)
  implicit val d: Decoder[Foo] = Decoder[String].map(Foo(_))

  implicit val ste: Encoder[SimpleTrait] = DerivedEncoder.gen
  implicit val std: Decoder[SimpleTrait] = DerivedDecoder.gen
}

@deriving(Encoder, Decoder) sealed abstract class AbstractThing(val id: String)
@deriving(Encoder, Decoder) case object Wibble extends AbstractThing("wibble")
@deriving(Encoder, Decoder) final case class Wobble(override val id: String)
    extends AbstractThing(id)

@deriving(Encoder, Decoder) final case class MultiField(a: String, b: String)
