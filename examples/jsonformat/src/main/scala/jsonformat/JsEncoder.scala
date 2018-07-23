// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import simulacrum._
import scalaz._, Scalaz._
import JsEncoder.ops._

@typeclass trait JsEncoder[A] {
  def toJson(obj: A): JsValue
}
object JsEncoder
    extends JsEncoderScalaz1
    with JsEncoderRefined
    with JsEncoderStdlib1
    with JsEncoderScalaz2
    with JsEncoderStdlib2
    with JsEncoderDeriving {

  implicit val contravariant: Contravariant[JsEncoder] =
    new Contravariant[JsEncoder] {
      def contramap[A, B](fa: JsEncoder[A])(f: B => A): JsEncoder[B] =
        b => fa.toJson(f(b))
    }

  implicit val jsValue: JsEncoder[JsValue] = identity
  implicit val long: JsEncoder[Long]       = JsInteger(_)
  implicit val double: JsEncoder[Double]   = JsDouble(_)
  implicit val boolean: JsEncoder[Boolean] = JsBoolean(_)
  implicit val string: JsEncoder[String]   = JsString(_)

  implicit val float: JsEncoder[Float]   = double.contramap(_.toDouble)
  implicit val int: JsEncoder[Int]       = long.contramap(_.toLong)
  implicit val short: JsEncoder[Short]   = long.contramap(_.toLong)
  implicit val byte: JsEncoder[Byte]     = long.contramap(_.toLong)
  implicit val unit: JsEncoder[Unit]     = long.contramap(_ => 1)
  implicit val char: JsEncoder[Char]     = string.contramap(_.toString)
  implicit val symbol: JsEncoder[Symbol] = string.contramap(_.name)

}

private[jsonformat] trait JsEncoderScalaz1 {
  this: JsEncoder.type =>

  implicit def foldable[F[_]: Foldable, A: JsEncoder]: JsEncoder[F[A]] =
    as => JsArray(as.toIList.map(_.toJson))

  implicit def maybe[A: JsEncoder]: JsEncoder[Maybe[A]] = {
    case Maybe.Just(a) => a.toJson
    case Maybe.Empty() => JsNull
  }
  implicit def disjunction[A: JsEncoder, B: JsEncoder]: JsEncoder[A \/ B] = {
    case -\/(a) => a.toJson
    case \/-(b) => b.toJson
  }
}
private[jsonformat] trait JsEncoderRefined {
  this: JsEncoder.type =>

  import eu.timepit.refined.api.Refined
  implicit def refined[A: JsEncoder, B]: JsEncoder[A Refined B] =
    JsEncoder[A].contramap(_.value)
}
private[jsonformat] trait JsEncoderStdlib1 {
  this: JsEncoder.type =>

  implicit def option[A: JsEncoder]: JsEncoder[Option[A]] =
    maybe[A].contramap(_.toMaybe)
  implicit def either[A: JsEncoder, B: JsEncoder]: JsEncoder[Either[A, B]] =
    disjunction[A, B].contramap(_.disjunction)

  implicit def dict[A: JsEncoder]: JsEncoder[Map[String, A]] = { m =>
    val fields = m.toList.map {
      case (k, v) => k -> v.toJson
    }
    JsObject(fields.toIList)
  }
}
private[jsonformat] trait JsEncoderScalaz2 {
  this: JsEncoder.type =>

}
private[jsonformat] trait JsEncoderStdlib2 {
  this: JsEncoder.type =>

  implicit def traversable[T[a] <: Traversable[a], A: JsEncoder]
    : JsEncoder[T[A]] =
    foldable[List, A].contramap(_.toList)
}

private[jsonformat] trait JsEncoderDeriving {
  this: JsEncoder.type =>

  implicit val deriving: Deriving[JsEncoder] = // scalafix:ok
    new Deriving[JsEncoder] {

      def xproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        name: String
      )(
        f: Prod[A] => Z,
        g: Z => Prod[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): JsEncoder[Z] = { z =>
        val fields = g(z).zip(tcs, labels).flatMap {
          case (label, a) /~\ fa =>
            fa.value.toJson(a) match {
              case JsNull => Nil
              case value  => (label -> value) :: Nil
            }
        }
        JsObject(fields.toIList)
      }

      def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        name: String
      )(
        f: Cop[A] => Z,
        g: Z => Cop[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): JsEncoder[Z] = { z =>
        g(z).zip(tcs, labels).into {
          case (label, a) /~\ fa =>
            val hint = "typehint" -> JsString(label)
            fa.value.toJson(a) match {
              case JsObject(fields) => JsObject(hint :: fields)
              case other            => JsObject(IList(hint, "xvalue" -> other))
            }
        }
      }

    }

}
