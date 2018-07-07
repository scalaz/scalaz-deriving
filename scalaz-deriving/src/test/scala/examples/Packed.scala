// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ Boolean, Double, Int, Long }

import scalaz._, Scalaz._
import simulacrum.typeclass

/**
 * Serialisation format that does not use field names.
 *
 * Note that Collection and Product are distinct in the sense that a schema may
 * define the length of Product whereas Collection is variable length.
 */
@deriving(Equal, Show)
sealed abstract class Packed {
  def widen: Packed = this
}
object Packed {
  final case class Real(double: Double)               extends Packed
  final case class Rational(long: Long)               extends Packed
  final case class Characters(chars: String)          extends Packed
  final case class Collection(entries: IList[Packed]) extends Packed
  final case class Product(entries: IList[Packed])    extends Packed
}
import Packed._

/**
 * Encoder for Packed.
 *
 * This is a warning that Decidablez instances for encoders produce LAWLESS
 * Divide.
 *
 * See https://github.com/scalaz/scalaz/issues/1907 for a minimised example.
 */
@typeclass trait BadPack[A] {
  def encode(a: A): Packed
}
object BadPack {
  implicit val long: BadPack[Long]     = i => Rational(i).widen
  implicit val string: BadPack[String] = i => Characters(i).widen

  implicit val decidablez: Decidablez[BadPack] = new Decidablez[BadPack] {
    def productz[Z, G[_]: Traverse](f: Z =*> G): BadPack[Z] = { z =>
      val entries = f(z).map { case fa /~\ a => fa.encode(a) }
      Product(entries.toIList).widen
    }

    def coproductz[Z](f: Z =+> Maybe): BadPack[Z] = { z =>
      f(z).into { case fa /~\ a => fa.encode(a) }
    }
  }

  // these don't do what you think they do...
  implicit val boolean: BadPack[Boolean] = long.contramap(b => if (b) 1 else 0)
  implicit val int: BadPack[Int]         = long.contramap(_.toLong)
}
