// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.examples

import java.lang.String
import scala.{ AnyVal, Int }

// https://github.com/playframework/play-json/issues/92
import scala.Predef.implicitly

import shapeless.{ LabelledGeneric => LG, Generic => G }
import stalactite.deriving
import stalactite.typeclasses._
import simulacrum.typeclass

import play.api.libs.json

import scalaz._
import Scalaz._

@deriving(G, LG, Cofoo, Cobar)
sealed trait Baz

@deriving(json.Format, Cofoo, Cobar, Wibble)
final case class Foo(string: String, int: Int) extends Baz

@deriving(G, LG, json.Format, Cofoo, Cobar)
final case class Bar(foo: Foo) extends Baz
object Bar {
  def hello: String = ""
}

@deriving(Cofoo)
private[examples] final case class Par(s: String)

// can't do json.Format: https://github.com/playframework/play-json/issues/93
// can't do G, LG https://github.com/milessabin/shapeless/issues/757
@deriving(Cofoo, Cobar)
case object Car extends Baz

@deriving(G, LG, Cofoo, Cobar)
final case class Anyz(s: String) extends AnyVal
@deriving(G, LG, Cofoo, Cobar)
final class Anyzz(val s: String) extends scala.AnyVal

@deriving(G, LG, json.Format)
final case class Gaz[T](t: T)

@typeclass trait Wibble[T] {}
object DerivedWibble {
  def gen[T]: Wibble[T] = new Wibble[T] {}
}
