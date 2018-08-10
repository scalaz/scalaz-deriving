// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import jsonformat._
import scalaz._, Scalaz._

@xderiving(Equal, Show, JsEncoder, JsDecoder)
final case class Optimal(thing: String) extends AnyVal

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed trait SimpleTrait { def widen: SimpleTrait = this }
final case class Foo(s: String) extends SimpleTrait
final case class Bar()          extends SimpleTrait
case object Baz                 extends SimpleTrait
@json(false, None, Some("fazzy"))
final case class Faz(
  @json(true, None, None) o: Option[String]
) extends SimpleTrait

@deriving(Equal, Show, JsEncoder, JsDecoder)
final case class Recursive(h: String, t: Option[Recursive] = None)

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed abstract class AbstractThing(val id: String) {
  def widen: AbstractThing = this
}
case object Wibble                               extends AbstractThing("wibble")
final case class Wobble(override val id: String) extends AbstractThing(id)

@deriving(Equal, Show, JsEncoder, JsDecoder)
@json(false, Some("TYPE"), None)
sealed abstract class NotAnObject { def widen: NotAnObject = this }
@xderiving(Equal, Show, JsEncoder, JsDecoder)
final case class Time(s: String) extends NotAnObject
final case class Money(
  @json(false, Some("integer"), None) i: Int
) extends NotAnObject

@deriving(Equal, Show, JsEncoder, JsDecoder)
sealed abstract class Zed { def widen: Zed = this }
@xderiving(Equal, Show, JsEncoder, JsDecoder)
@json(false, Some("z"), Some("ded"))
final case class Dead(baby: String) extends Zed

@deriving(Equal, Show, JsEncoder, JsDecoder)
final case class CanHasDefaults(foo: String = "cheez")
