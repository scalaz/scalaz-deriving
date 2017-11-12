// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Boolean, Int }
import scala.collection.immutable.Stream

import Scalaz._

// a simple covariant typeclass
trait Default[A] {
  def default: A
}
object Default {
  @inline def apply[A](implicit i: Default[A]): Default[A] = i
  @inline def instance[A](a: => A): Default[A] = new Default[A] {
    override def default: A = a
  }

  implicit val int: Default[Int]         = instance(0)
  implicit val string: Default[String]   = instance("")
  implicit val boolean: Default[Boolean] = instance(false)

  implicit val Derived: Derived[Default] = new CovariantDerived[Default] {
    val extract = λ[Default ~> Id](_.default)
    override def products[Z](f: (Default ~> Id) => Z): Default[Z] =
      instance { f(extract) }

    val choose = λ[Default ~> Maybe](_.default.just)
    override def coproducts[Z](f: (Default ~> Maybe) => Stream[Z]): Default[Z] =
      instance { f(choose).head }
    // .head is safe for Default because at least one thing is chosen
  }

}
