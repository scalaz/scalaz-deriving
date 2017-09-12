// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.examples

import java.lang.String
import scala.{ AnyVal, Either, Int }

// https://github.com/playframework/play-json/issues/92
import scala.Predef.implicitly

import stalactite.deriving
import stalactite.typeclasses.{ Cobar => B, _ }
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
private[examples] final case class Par(s: String)

// can't do json.Format: https://github.com/playframework/play-json/issues/93
@deriving(Cofoo, B)
case object Car extends Baz

@deriving(Cofoo, B)
final case class Anyz(s: String) extends AnyVal
@deriving(Cofoo, B)
final class Anyzz(val s: String) extends scala.AnyVal
@deriving(Cofoo)
final class Valuezz[L, R](val e: Either[L, R]) extends AnyVal

@deriving(json.Format)
final case class Gaz[T](t: T)

@typeclass trait Wibble[T] {}
object DerivedWibble {
  def gen[T]: Wibble[T] = new Wibble[T] {}
}
