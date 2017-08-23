// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalagtite

import org.scalatest._
import Matchers._
import OptionValues._
import org.ensime.pcplod._

class PresentationCompilerTest extends FlatSpec {
  // the macro is not being run
  "PresentationCompiler" should "see generated implicits" ignore withMrPlod(
    "interactive.scala"
  ) { mr =>
    //scala.Predef.println(mr.pc.pc.settings)

    mr.messages shouldBe 'empty

    mr.typeAtPoint('foo).value shouldBe "wibble.Foo"
    mr.typeAtPoint('baz).value shouldBe "wibble.Baz"
    mr.typeAtPoint('gaz).value shouldBe "wibble.Gaz[T]"
  }
}
