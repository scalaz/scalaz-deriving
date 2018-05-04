// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._
import simulacrum._

/** Encoder for the XString half of the XNode ADT. */
@typeclass trait XStrEncoder[A] { self =>
  def toXml(a: A): XString
}
object XStrEncoder extends XStrEncoderScalaz with XStrEncoderStdlib {

  implicit val contravariant: Contravariant[XStrEncoder] =
    new Contravariant[XStrEncoder] {
      def contramap[A, B](fa: XStrEncoder[A])(f: B => A): XStrEncoder[B] =
        b => fa.toXml(f(b))
    }

  // JVM data types
  implicit val boolean: XStrEncoder[Boolean]           = a => XAtom(a.toString)
  implicit val short: XStrEncoder[Short]               = a => XAtom(a.toString)
  implicit val int: XStrEncoder[Int]                   = a => XAtom(a.toString)
  implicit val long: XStrEncoder[Long]                 = a => XAtom(a.toString)
  implicit val float: XStrEncoder[Float]               = a => XAtom(a.toString)
  implicit val double: XStrEncoder[Double]             = a => XAtom(a.toString)
  implicit val uuid: XStrEncoder[java.util.UUID]       = a => XAtom(a.toString)
  implicit val instant: XStrEncoder[java.time.Instant] = a => XAtom(a.toString)
  implicit val string: XStrEncoder[String]             = s => XText(s)
  implicit val char: XStrEncoder[Char]                 = string.contramap(_.toString)
  implicit val symbol: XStrEncoder[Symbol]             = string.contramap(_.name)

  // trivial
  implicit val xcdata: XStrEncoder[XCdata] = identity
}

trait XStrEncoderScalaz {
  this: XStrEncoder.type =>

  implicit def disjunction[
    A: XStrEncoder,
    B: XStrEncoder
  ]: XStrEncoder[A \/ B] = {
    case -\/(a) => XStrEncoder[A].toXml(a)
    case \/-(b) => XStrEncoder[B].toXml(b)
  }

}

trait XStrEncoderStdlib {
  this: XStrEncoder.type =>

  implicit def either[
    A: XStrEncoder,
    B: XStrEncoder
  ]: XStrEncoder[Either[A, B]] = disjunction[A, B].contramap(_.disjunction)

  import scala.concurrent.duration.FiniteDuration
  implicit def finite: XStrEncoder[FiniteDuration] = long.contramap(_.toMillis)
}
