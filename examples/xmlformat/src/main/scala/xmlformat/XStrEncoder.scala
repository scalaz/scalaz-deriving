// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._
import simulacrum._

/** Encoder for the XString half of the XNode ADT. */
@typeclass trait XStrEncoder[A] { self =>
  def toXml(a: A): XString

  // for performance
  final def xmap[B](@unused f: A => B, g: B => A): XStrEncoder[B] = contramap(g)
  final def contramap[B](g: B => A): XStrEncoder[B]               = b => toXml(g(b))
}
object XStrEncoder
    extends XStrEncoderScalaz
    with XStrEncoderRefined
    with XStrEncoderStdlib      {

  implicit val contravariant: Contravariant[XStrEncoder] =
    new Contravariant[XStrEncoder] {
      def contramap[A, B](fa: XStrEncoder[A])(f: B => A): XStrEncoder[B] =
        b => fa.toXml(f(b))
    }

  // JVM data types
  implicit val string: XStrEncoder[String]             = s => XString(s)
  implicit val boolean: XStrEncoder[Boolean]           = string.contramap(_.toString)
  implicit val short: XStrEncoder[Short]               = string.contramap(_.toString)
  implicit val int: XStrEncoder[Int]                   = string.contramap(_.toString)
  implicit val long: XStrEncoder[Long]                 = string.contramap(_.toString)
  implicit val float: XStrEncoder[Float]               = string.contramap(_.toString)
  implicit val double: XStrEncoder[Double]             = string.contramap(_.toString)
  implicit val uuid: XStrEncoder[java.util.UUID]       = string.contramap(_.toString)
  implicit val instant: XStrEncoder[java.time.Instant] =
    string.contramap(_.toString)
  implicit val char: XStrEncoder[Char]                 = string.contramap(_.toString)
  implicit val symbol: XStrEncoder[Symbol]             = string.contramap(_.name)

  implicit val xstring: XStrEncoder[XString] = identity
}

private[xmlformat] trait XStrEncoderScalaz {
  this: XStrEncoder.type =>

  implicit def disjunction[
    A: XStrEncoder,
    B: XStrEncoder
  ]: XStrEncoder[A \/ B] = {
    case -\/(a) => XStrEncoder[A].toXml(a)
    case \/-(b) => XStrEncoder[B].toXml(b)
  }

}

// WORKAROUND https://github.com/scala/bug/issues/10753
private[xmlformat] trait XStrEncoderRefined {
  this: XStrEncoder.type =>

  import eu.timepit.refined.api.Refined
  implicit def refined[A: XStrEncoder, B]: XStrEncoder[A Refined B] =
    XStrEncoder[A].contramap(_.value)

}

private[xmlformat] trait XStrEncoderStdlib {
  this: XStrEncoder.type =>

  implicit def either[
    A: XStrEncoder,
    B: XStrEncoder
  ]: XStrEncoder[Either[A, B]] = disjunction[A, B].contramap(_.toDisjunction)

  import scala.concurrent.duration.FiniteDuration
  implicit def finite: XStrEncoder[FiniteDuration] = long.contramap(_.toMillis)
}
