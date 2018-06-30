// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Boolean, Int }

import Scalaz._

// a simple covariant typeclass
trait Default[A] {
  def default: A
}
object Default {
  @inline def apply[A](implicit i: Default[A]): Default[A] = i
  @inline def instance[A](a: =>A): Default[A] = new Default[A] {
    override def default: A = a
  }

  implicit val int: Default[Int]         = instance(0)
  implicit val string: Default[String]   = instance("")
  implicit val boolean: Default[Boolean] = instance(false)

  implicit val instances: CovariantDeriving[Default] =
    new CovariantDeriving[Default] {
      val extract: Default ~> Id = λ[Default ~> Id](a => a.default)
      override def productz[Z](
        f: (Default ~> Id) => Z
      ): Default[Z] =
        instance { f(extract) }

      val always: Default ~> Maybe = λ[Default ~> Maybe](_.default.just)
      override def coproductz[Z](
        f: (Default ~> Maybe) => EphemeralStream[Z]
      ): Default[Z] =
        instance { f(always).head() }
      // NOTE: the typeclass does not allow failure, so we are forced to perform
      // an unsafe .head() call. We should fix this by making Default return a
      // Maybe.
    }

}
