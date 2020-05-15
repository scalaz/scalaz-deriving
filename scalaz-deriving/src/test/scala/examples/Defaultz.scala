// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ inline, Boolean, Int }
import scalaz._, Scalaz._

// like Default, but with an Altz
trait Defaultz[A] {
  def default: A
}
object Defaultz   {
  @inline def apply[A](implicit i: Defaultz[A]): Defaultz[A] = i
  @inline def instance[A](a: =>A): Defaultz[A]               =
    new Defaultz[A] {
      override def default: A = a
    }

  implicit val int: Defaultz[Int]         = instance(0)
  implicit val string: Defaultz[String]   = instance("")
  implicit val boolean: Defaultz[Boolean] = instance(false)

  implicit val defaultz_altz: Altz[Defaultz] = new Altz[Defaultz] {
    private val extract = λ[NameF ~> Id](a => a.value.default)
    def applyz[Z, A <: TList, FA <: TList](tcs: Prod[FA])(f: Prod[A] => Z)(
      implicit ev: A PairedWith FA
    ): Defaultz[Z]      =
      instance {
        f(tcs.traverse[A, NameF, Id](extract))
      }

    private val always =
      λ[NameF ~> Maybe](a => Maybe.just(a.value.default))
    def altlyz[Z, A <: TList, FA <: TList](tcs: Prod[FA])(f: Cop[A] => Z)(
      implicit ev: A PairedWith FA
    ): Defaultz[Z]     =
      instance {
        val head = tcs.coptraverse[A, NameF, Id](always).headMaybe match {
          case Maybe.Empty() => scala.sys.error("I am sorry, I am not Total.")
          case Maybe.Just(a) => a
        }
        f(head)
      }
  }

}
