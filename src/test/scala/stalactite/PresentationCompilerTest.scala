// Copyright: 2017 https://gitlab.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalagtite

import org.scalatest._
import Matchers._
import OptionValues._
import org.ensime.pcplod._

class PresentationCompilerTest extends FlatSpec {
  // the macro is not being run
  "PresentationCompiler" should "not have errors" in withMrPlod(
    "interactive.scala"
  ) { mr =>
    mr.messages shouldBe 'empty
  }

  it should "be able to perform type-at-point" ignore withMrPlod(
    "interactive.scala"
  ) { mr =>
    mr.typeAtPoint('foo).value shouldBe "wibble.Foo"
    mr.typeAtPoint('baz).value shouldBe "wibble.Baz"
    mr.typeAtPoint('gaz).value shouldBe "wibble.Gaz[T]"
  }
}
