// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.lang.String
import java.math.{ BigDecimal => BD }

import scala.Predef.identity
import scala.collection.immutable.{ Map, Seq, Traversable }
import scala.concurrent.duration.FiniteDuration
import scala.xml._

import scalaz.{ Node => _, _ }
import scalaz.Scalaz._
import simulacrum._

/**
 * Encodes arbitrary types into the `scala.xml` ADT.
 *
 * Note that the `NodeSeq` trait is unsealed. As of scala-xml 1.0.6, the
 * known implementations are:
 *
 * - Atom
 * - Comment
 * - Document
 * - Elem
 * - EntityRef
 * - Group
 * - PCData
 * - ProcInstr
 * - SpecialNodeSeq
 * - Text
 * - Unparsed
 *
 * In lieu of https://github.com/propensive/xylophone
 */
@typeclass
trait Encoder[A] { self =>
  def toXml(a: A): NodeSeq

  /**
   * Create a new [[Encoder]] by applying a function to a value of
   * type `B` before encoding as an `A`.
   */
  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    final def toXml(a: B) = self.toXml(f(a))
  }
}

object Encoder {
  def instance[A](f: A => NodeSeq): Encoder[A] = new Encoder[A] {
    override def toXml(a: A): NodeSeq = f(a)
  }
  // only for values that definitely do not require escaping
  private def atomic[A]: Encoder[A] = new Encoder[A] {
    override def toXml(a: A): NodeSeq = new Atom(a.toString)
  }

  // AnyVal support
  implicit val invariant: InvariantFunctor[Encoder] =
    new InvariantFunctor[Encoder] {
      override def xmap[A, B](ma: Encoder[A],
                              f: A => B,
                              g: B => A): Encoder[B] =
        ma.contramap(g)
    }

  def el(name: String, els: Seq[Node]) =
    Elem(null, name, Null, TopScope, true, els: _*)

  import ops._

  implicit def nodeSeq[X <: NodeSeq]: Encoder[X] = instance(identity)

  implicit val boolean: Encoder[Boolean] = atomic

  // with the caveat that large numbers can cause exceptions.
  // BigInteger not supported: a JVM issue can freeze Threads.
  implicit val bigDecimal: Encoder[BD] = atomic
  implicit val short: Encoder[Short]   = atomic
  implicit val int: Encoder[Int]       = atomic
  implicit val long: Encoder[Long]     = atomic
  implicit val float: Encoder[Float]   = atomic
  implicit val double: Encoder[Double] = atomic

  implicit val string: Encoder[String] = instance(new Text(_))
  implicit val char: Encoder[Char] =
    Encoder[String].contramap(_.toString)
  implicit val symbol: Encoder[Symbol] =
    Encoder[String].contramap(_.name)

  // special-case Option to remove redundancy
  implicit def option[A: Encoder]: Encoder[Option[A]] = instance {
    case Some(a) => Group(a.toXml.theSeq)
    case None    => Group(Nil)
  }

  // special-case Either to remove redundancy
  implicit def either[A: Encoder, B: Encoder]: Encoder[Either[A, B]] =
    instance {
      case Left(a)  => el("Left", a.toXml.theSeq)
      case Right(b) => el("Right", b.toXml.theSeq)
    }

  // special-case Validated to remove redundancy
  implicit def validated[
    A: Encoder,
    B: Encoder
  ]: Encoder[Validation[A, B]] =
    instance {
      case Failure(a) => el("Failure", a.toXml.theSeq)
      case Success(b) => el("Success", b.toXml.theSeq)
    }

  implicit def nel[A: Encoder]: Encoder[NonEmptyList[A]] = instance { t =>
    Group(t.toList.map(s => el("value", s.toXml)))
  }

  // special-case Map, not as a Traversable[(K, V)]
  implicit def dict[K: Encoder, V: Encoder]: Encoder[Map[K, V]] =
    instance { ss =>
      val contents: Seq[Node] = ss.map {
        case (key, value) =>
          el(
            "entry",
            el("key", key.toXml) :: el("value", value.toXml) :: Nil
          )
      }(collection.breakOut)
      Group(contents)
    }

  implicit val finiteDuration: Encoder[FiniteDuration] =
    long.contramap[FiniteDuration](_.toMillis)

  // evidence that T is a subtype of Traversable.
  implicit def traversable[T[a] <: Traversable[a], A: Encoder]
    : Encoder[T[A]] = { ss =>
    Group(ss.map(s => el("value", Encoder[A].toXml(s)))(collection.breakOut))
  }

}
