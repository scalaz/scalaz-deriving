// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package testing.classes

import java.lang.String
import scala.{ AnyVal, Either, Int }

import scalaz.deriving
import testing.typeclasses.{ Cobar => B, _ }
import simulacrum.typeclass

import play.api.libs.json

import shapeless.{ Generic, LabelledGeneric }

import scalaz._
import Scalaz._

@deriving(Cofoo, B)
sealed trait Baz

@deriving(json.Format, Cofoo, B, Wibble)
final case class Foo(string: String, int: Int) extends Baz

@deriving(Generic, LabelledGeneric, json.Format, Cofoo, B)
final case class Bar(foo: Foo) extends Baz
object Bar {
  def hello: String = ""
}

@deriving(Cofoo)
private[testing] final case class Par(s: String)

// can't do json.Format: https://github.com/playframework/play-json/issues/93
@deriving(Generic, LabelledGeneric, Cofoo, B)
case object Car extends Baz

@xderiving(Cofoo, B)
final case class Anyx(s: String) extends AnyVal
@deriving(Cofoo, B)
final case class Anyz(s: String) extends AnyVal

@xderiving(Cofoo, B)
final class Anyzz(val s: String) extends scala.AnyVal
@xderiving(Cofoo)
final class Valuezz[L, R](val e: Either[L, R]) extends AnyVal

@deriving(json.Format, Generic, LabelledGeneric)
final case class Gaz[T](t: T)

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
}
