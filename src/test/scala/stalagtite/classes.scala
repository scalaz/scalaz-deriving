// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.examples

import java.lang.String
import scala.Int

// https://github.com/playframework/play-json/issues/92
import scala.Predef.implicitly

import stalactite.deriving
import stalactite.typeclasses._
import simulacrum.typeclass

import play.api.libs.json

@deriving(Cofoo, Cobar)
sealed trait Baz

@deriving(json.Format, Cofoo, Cobar, Wibble)
final case class Foo(string: String, int: Int) extends Baz

@deriving(json.Format, Cofoo, Cobar)
final case class Bar(foo: Foo) extends Baz
object Bar {
  def hello: String = ""
}

// can't do json.Format: https://github.com/playframework/play-json/issues/93
@deriving(Cofoo, Cobar)
case object Car extends Baz

//@deriving(json.Format, a.Cobaz, b.Cobaz)
@deriving(json.Format)
final case class Gaz[T](t: T)

@typeclass trait Wibble[T] {}
object DerivedWibble {
  def gen[T]: Wibble[T] = new Wibble[T] {}
}
