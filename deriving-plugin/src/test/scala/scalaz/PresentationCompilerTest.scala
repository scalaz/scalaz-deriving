// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.Symbol
import org.scalatest._
import Matchers._
import OptionValues._
import org.ensime.pcplod._

class PresentationCompilerTest extends FlatSpec {
  "PresentationCompiler" should "not have errors" in withMrPlod(
    "interactive.scala"
  ) { mr =>
    mr.messages.shouldBe(Symbol("empty"))
  }

  it should "be able to perform type-at-point" in withMrPlod(
    "interactive.scala"
  ) { mr =>
    mr.typeAtPoint(Symbol("foo")).value.shouldBe("wibble.Foo")
    mr.typeAtPoint(Symbol("baz")).value.shouldBe("wibble.Baz")
    mr.typeAtPoint(Symbol("gaz")).value.shouldBe("wibble.Gaz[T]")
  }
}
