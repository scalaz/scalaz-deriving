// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.examples

import java.lang.String
import scala.Int
import scala.annotation.StaticAnnotation

// https://github.com/playframework/play-json/issues/92
import scala.Predef.implicitly

import stalactite.deriving
import simulacrum.typeclass

import play.api.libs.json

class remove extends StaticAnnotation

@deriving(Cofoo)
sealed trait Baz
@remove object Baz {
  @remove implicit val `stalactite.examples.Cofoo[Baz]` : Cofoo[Baz] =
    DerivedCofoo.gen
  @remove implicit val `stalactite.examples.Cobar[Baz]` : Cobar[Baz] =
    CustomCobar.go
}

@deriving(json.Format, Cofoo)
final case class Foo(string: String, int: Int) extends Baz
@remove object Foo {
  @remove implicit val `play.api.libs.json.Format`: json.Format[Foo] =
    json.Json.format
  @remove implicit val `stalactite.examples.Cofoo`: Cofoo[Foo] =
    DerivedCofoo.gen
  @remove implicit val `stalactite.examples.Cobar`: Cobar[Foo] =
    CustomCobar.go
}

@deriving(json.Format, Cofoo)
final case class Bar(foo: Foo) extends Baz
object Bar {
  def hello: String = ""

  @remove implicit val `play.api.libs.json.Format`: json.Format[Bar] =
    json.Json.format
  @remove implicit val `stalactite.examples.Cofoo`: Cofoo[Bar] =
    DerivedCofoo.gen
  @remove implicit val `stalactite.examples.Cobar`: Cobar[Bar] =
    CustomCobar.go
}

@deriving(json.Format, a.Cobaz, b.Cobaz)
final case class Gaz[T](t: T)
@remove object Gaz {
  @remove implicit def `play.api.libs.json.Format[Gaz]`[
    T: json.Format
  ]: json.Format[Gaz[T]] = json.Json.format

  @remove implicit def `stalactite.examples.a.Cobaz`: a.Cobaz[Gaz] =
    a.DerivedCobaz.gen
  @remove implicit def `stalactite.examples.b.Cobaz`: b.Cobaz[Gaz] =
    b.DerivedCobaz.gen
}

@typeclass
trait Cofoo[T]
trait DerivedCofoo[T] extends Cofoo[T]
object DerivedCofoo {
  def gen[T, Repr](
    implicit G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): DerivedCofoo[T] = new DerivedCofoo[T] {}
}

@typeclass
trait Cobar[T]
object CustomCobar {
  def go[T, Repr](
    implicit G: shapeless.LabelledGeneric.Aux[T, Repr]
  ): Cobar[T] = new Cobar[T] {}
}

package a {
  @typeclass trait Cobaz[T[_]]
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T]
  object DerivedCobaz {
    def gen[T[_]](
      implicit G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null
  }
}
package b {
  @typeclass trait Cobaz[T[_]]
  @typeclass trait DerivedCobaz[T[_]] extends Cobaz[T]
  object DerivedCobaz {
    def gen[T[_]](
      implicit G: shapeless.Generic1[T, DerivedCobaz]
    ): DerivedCobaz[T] =
      new DerivedCobaz[T] {}

    implicit def hcons[F[_]]: DerivedCobaz[F] = null
  }
}
