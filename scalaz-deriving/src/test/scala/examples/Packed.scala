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
    def dividez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Prod[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): BadPack[Z] = { z =>
      val entries = g(z).zip(tcs).map {
        case a /~\ fa => fa.value.encode(a)
      }
      Product(entries.toIList)
    }

    def choosez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Cop[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): BadPack[Z] = { z =>
      g(z).zip(tcs).into {
        case a /~\ fa => fa.value.encode(a)
      }
    }

  }

  // these don't do what you think they do...
  implicit val boolean: BadPack[Boolean] = long.contramap(b => if (b) 1 else 0)
  implicit val int: BadPack[Int]         = long.contramap(_.toLong)
}
