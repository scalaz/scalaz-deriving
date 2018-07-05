// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ inline, Boolean, Int }

import scalaz._, Scalaz._
import EphemeralStream.EStream

// like Defaultz, but returns Maybe
trait Defaultzy[A] {
  def default: Maybe[A]
}
object Defaultzy {
  @inline def apply[A](implicit i: Defaultzy[A]): Defaultzy[A] = i
  @inline def instance[A](ma: =>Maybe[A]): Defaultzy[A] = new Defaultzy[A] {
    override def default: Maybe[A] = ma
  }

  implicit val int: Defaultzy[Int]         = instance(0.just)
  implicit val string: Defaultzy[String]   = instance("".just)
  implicit val boolean: Defaultzy[Boolean] = instance(false.just)

  implicit val defaultz_altz: Altz[Defaultzy] = new Altz[Defaultzy] {
    type G[a] = Maybe[a]
    def G: Applicative[Maybe] = Applicative[Maybe]

    val extract: Defaultzy ~> Maybe = λ[Defaultzy ~> Maybe](a => a.default)
    override def productz[Z](
      f: (Defaultzy ~> Maybe) => Maybe[Z]
    ): Defaultzy[Z] =
      instance(f(extract))

    val always: Defaultzy ~> EStream =
      λ[Defaultzy ~> EStream](a => a.default.toEphemeralStream)
    override def coproductz[Z](
      f: (Defaultzy ~> EStream) => EStream[Z]
    ): Defaultzy[Z] =
      instance(f(always).headOption.toMaybe)
  }

}
