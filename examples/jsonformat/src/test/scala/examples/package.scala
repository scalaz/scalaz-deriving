// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import jsonformat._
import scalaz._, Scalaz._

@xderiving(Equal, Show, JsEncoder, JsDecoder)
final case class Optimal(thing: String) extends AnyVal

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed trait SimpleTrait
final case class Foo(s: String)         extends SimpleTrait
final case class Bar()                  extends SimpleTrait
case object Baz                         extends SimpleTrait
final case class Faz(o: Option[String]) extends SimpleTrait

@deriving(Equal, Show, JsEncoder, JsDecoder)
final case class Recursive(h: String, t: Option[Recursive] = None)

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed abstract class AbstractThing(val id: String)
case object Wibble                               extends AbstractThing("wibble")
final case class Wobble(override val id: String) extends AbstractThing(id)

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed abstract class NotAnObject
@xderiving(Equal, Show, JsEncoder, JsDecoder)
final case class Time(s: String) extends NotAnObject
final case class Money(i: Int)   extends NotAnObject
