// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ inline, Boolean, Int }
import scalaz._, Scalaz._

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
    private val extract = λ[NameF ~> Maybe](a => a.value.default)
    def applyz[Z, A <: TList, TC <: TList](tcs: Prod[TC])(
      f: Prod[A] => Z
    )(
      implicit ev1: NameF ƒ A ↦ TC
    ): Defaultzy[Z] = instance {
      tcs.traverse[A, NameF, Maybe](extract).map(f)
    }

    private val always =
      λ[NameF ~> Maybe](a => a.value.default)
    def altlyz[Z, A <: TList, TC <: TList](tcs: Prod[TC])(
      f: Cop[A] => Z
    )(
      implicit ev1: NameF ƒ A ↦ TC
    ): Defaultzy[Z] = instance {
      tcs.coptraverse[A, NameF, Id](always).map(f).headMaybe.toMaybe
    }
  }

}
