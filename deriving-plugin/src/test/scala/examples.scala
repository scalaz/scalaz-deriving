/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package testing

import java.lang.String
import scala.{ AnyVal, Either, Int }

import scalaz.annotation.{ deriving, xderiving }

package typeclasses {
  trait Cofoo[A]
  object Cofoo
  trait Cobar[A]
  object Cobar
  package json {
    trait Format[A]
    object Format {
      implicit val string: Format[String] = null // scalafix:ok
    }
  }
}
trait Wibble[A]
object Wibble
import typeclasses.{ Cobar => B, _ }

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
final class Anyzz(val s: String)               extends scala.AnyVal
@xderiving(Cofoo)
final class Valuezz[L, R](val e: Either[L, R]) extends AnyVal

@scalaz.annotation.deriving(json.Format)
final case class Gaz[T](t: T)

@deriving(d.ValForwarder)
final case class D(i: Int)
package object d {
  val ValForwarder: Cofoo.type = Cofoo
  type ValForwarder[a] = Cofoo[a]
}

@deriving
final case class Si3664(foo: String)

@deriving(Cofoo)
sealed abstract class Duped
object Duped {
  @deriving(Cofoo)
  final case class Souped(i: Int) extends Duped
}

package nesty {
  @deriving(Cofoo)
  final case class Duped(s: String)
}
