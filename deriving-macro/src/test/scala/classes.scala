// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package testing.classes

import java.lang.String
import scala.{ AnyVal, Either, Int }

import scalaz.deriving
import testing.typeclasses.{ Cobar => B, _ }
import simulacrum.typeclass

import play.api.libs.json

import scalaz._
import Scalaz._

@deriving(Cofoo, B)
sealed trait Baz

@deriving(json.Format, Cofoo, B, Wibble)
final case class Foo(string: String, int: Int) extends Baz

@deriving(json.Format, Cofoo, B)
final case class Bar(foo: Foo) extends Baz
object Bar {
  def hello: String = ""
}

@deriving(Cofoo)
private[testing] final case class Par(s: String)

@deriving(Cofoo, B)
case object Car extends Baz

@xderiving(Cofoo, B)
final case class Van(v: String) extends Baz

@xderiving(Cofoo, B)
final case class Anyx(s: String) extends AnyVal
@deriving(Cofoo, B)
final case class Anyz(s: String) extends AnyVal

@xderiving(Cofoo, B)
final class Anyzz(val s: String) extends scala.AnyVal
@xderiving(Cofoo)
final class Valuezz[L, R](val e: Either[L, R]) extends AnyVal

// BUG https://gitlab.com/fommil/scalaz-deriving/issues/65
@deriving(json.Format)
final case class Gaz[T](t: T)

@deriving(Cofoo)
final class Waz[T](val t: T)

@typeclass trait Wibble[T] {}
object DerivedWibble {
  def gen[T]: Wibble[T] = new Wibble[T] {}
}

@deriving(CustomGen)
final case class C(i: Int)

@deriving(d.ValForwarder)
final case class D(i: Int)
package object d {
  val ValForwarder = Cofoo
  type ValForwarder[a] = Cofoo[a]
}
