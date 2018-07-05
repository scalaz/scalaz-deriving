// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ inline, Boolean, Int }

import scalaz._, Scalaz._
import EphemeralStream.EStream

// like Default, but with an Altz
trait Defaultz[A] {
  def default: A
}
object Defaultz {
  @inline def apply[A](implicit i: Defaultz[A]): Defaultz[A] = i
  @inline def instance[A](a: =>A): Defaultz[A] = new Defaultz[A] {
    override def default: A = a
  }

  implicit val int: Defaultz[Int]         = instance(0)
  implicit val string: Defaultz[String]   = instance("")
  implicit val boolean: Defaultz[Boolean] = instance(false)

  implicit val defaultz_altz: Altz[Defaultz] = new Altz[Defaultz] {
    type G[a] = Id[a]
    def G: Applicative[Id] = Applicative[Id]

    val extract: Defaultz ~> Id = λ[Defaultz ~> Id](a => a.default)
    override def productz[Z](f: (Defaultz ~> Id) => Z): Defaultz[Z] =
      instance(f(extract))

    val always: Defaultz ~> EStream =
      λ[Defaultz ~> EStream](a => EphemeralStream(a.default))
    override def coproductz[Z](
      f: (Defaultz ~> EStream) => EStream[Z]
    ): Defaultz[Z] =
      instance(f(always).headOption.get)
  }

}
