// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ inline, AnyVal, Boolean, Double, Int, Long, Some }

import scalaz._, Scalaz._, Isomorphism.<~>, EphemeralStream.EStream

import simulacrum.typeclass

import NamePacker.ops._
import NameUnpacker.ops._

/** Simple serialisation format that uses field names. */
@deriving(Equal, Show)
sealed abstract class NamePacked {
  def widen: NamePacked = this
}
object NamePacked {
  final case class Real(double: Double)                   extends NamePacked
  final case class Rational(long: Long)                   extends NamePacked
  final case class Characters(chars: String)              extends NamePacked
  final case class Collection(entries: IList[NamePacked]) extends NamePacked
  final case class Product(entries: IList[(String, NamePacked)])
      extends NamePacked
}
import NamePacked._

/** Encoder for NamePacked. */
@typeclass trait NamePacker[A] {
  def encode(a: A): NamePacked
}
object NamePacker {
  implicit val contravariant: Contravariant[NamePacker] =
    new Contravariant[NamePacker] {
      def contramap[A, B](fa: NamePacker[A])(f: B => A): NamePacker[B] =
        b => fa.encode(f(b))
    }

  implicit val long: NamePacker[Long]     = i => Rational(i)
  implicit val string: NamePacker[String] = i => Characters(i)
  implicit val double: NamePacker[Double] = i => Real(i)
  implicit val boolean: NamePacker[Boolean] =
    long.contramap(b => if (b) 1 else 0)
  implicit val int: NamePacker[Int] = long.contramap(_.toLong)

  implicit def foldable[F[_]: Foldable, A: NamePacker]: NamePacker[F[A]] =
    as => Collection(as.toIList.map(_.encode))

  implicit val encoder: LabelledEncoder[NamePacker] =
    new LabelledEncoder[NamePacker] {

      def productz[Z, G[_]: Traverse](f: Z =*> G): NamePacker[Z] = { z: Z =>
        val els = f(z).map { case fa /~\ ((label, a)) => (label, fa.encode(a)) }
        Product(els.toIList)
      }

      def coproductz[Z](f: Z =+> Maybe): NamePacker[Z] = { z: Z =>
        f(z) match {
          case fa /~\ ((label, a)) =>
            val hint = ("typehint", string.encode(label))
            fa.encode(a) match {
              case Product(entries) => Product(hint :: entries)
              case other            => Product(hint :: ("value", other) :: IList.empty)
            }
        }
      }

    }
}

/** Decoder for NamePacked. */
@typeclass(generateAllOps = false) trait NameUnpacker[A] {
  def decode(p: NamePacked): Cord \/ A
}
object NameUnpacker {
  object ops extends ToNameUnpackerOps {
    implicit class Ops(private val p: NamePacked) extends AnyVal {
      def decode[A: NameUnpacker]: Cord \/ A = NameUnpacker[A].decode(p)
    }
  }

  def fail[A](expected: String, got: NamePacked): -\/[Cord] =
    -\/("expected " +: expected +: ", got " +: got.show)

  @inline def instance[A](f: NamePacked => Cord \/ A): NameUnpacker[A] = f(_)
  val iso: NameUnpacker <~> Kleisli[Cord \/ ?, NamePacked, ?] = Kleisli.iso(
    λ[λ[a => (NamePacked => Cord \/ a)] ~> NameUnpacker](instance(_)),
    λ[NameUnpacker ~> λ[a => (NamePacked => Cord \/ a)]](_.decode)
  )
  implicit val monad: MonadError[NameUnpacker, Cord] = MonadError.fromIso(iso)

  implicit val long: NameUnpacker[Long] = {
    case Rational(i) => i.right
    case other       => fail("Rational", other)
  }
  implicit val string: NameUnpacker[String] = {
    case Characters(i) => i.right
    case other         => fail("Characters", other)
  }
  implicit val double: NameUnpacker[Double] = {
    case Real(i) => i.right
    case other   => fail("Real", other)
  }

  implicit val boolean: NameUnpacker[Boolean] = long.emap { i =>
    if (i == 0) false.right
    else if (i == 1) true.right
    else ("expected 0 or 1, got " +: i.show).left
  }
  implicit val int: NameUnpacker[Int] = long.emap { i =>
    if (i >= Int.MinValue && i <= Int.MaxValue) i.toInt.right
    else ("expected 32 bit signed integer, got (64 bit) " +: i.show).left
  }

  // must special case data foldable things because there is no FromFoldable...
  implicit def ilist[A: NameUnpacker]: NameUnpacker[IList[A]] = {
    case Collection(entries) => entries.traverse(_.decode[A])
    case other               => fail("Collection", other)
  }

  implicit val decoder: LabelledDecoder[NameUnpacker] =
    new LabelledDecoder[NameUnpacker] {
      type G[a] = Cord \/ a
      def G: Applicative[G] = Applicative[G]

      override def productz[Z](
        f: (LFA ~> G) => G[Z]
      ): NameUnpacker[Z] = {
        case Product(entries) =>
          val each = λ[LFA ~> (Cord \/ ?)] {
            case (label, fa) =>
              entries
                .find(_._1 == label)
                .cata(
                  e => fa.decode(e._2),
                  ("expected field " +: label.show :+ ", got " + entries
                    .map(_._1)
                    .show).left
                )
          }
          f(each)

        case other => fail("Product", other)
      }

      override def coproductz[Z](
        f: (LFA ~> EStream) => EStream[Z]
      ): NameUnpacker[Z] = {
        case p @ Product(entries) =>
          entries.find(_._1 == "typehint") match {
            case Some((_, Characters(hint))) =>
              val each = λ[LFA ~> EStream] {
                case (label, fa) =>
                  if (hint == label)
                    fa.decode(p).toEphemeralStream
                  else EphemeralStream()
              }
              f(each).headOption \/> ("a valid " +: hint.show)

            case _ =>
              ("expected \"typehint\", got " +: entries.map(_._1).show).left
          }

        case other => fail("Product", other)
      }
    }
}
