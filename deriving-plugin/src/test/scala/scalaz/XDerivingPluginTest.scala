// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.Predef.implicitly

import org.scalatest._
import org.scalatest.Matchers._
import testing._
import testing.typeclasses._

class XDerivingTest extends FlatSpec {
  // scalafix:off DisableSyntax.keywords.null

  "@xderiving" should "support AnyVal" in {
    implicitly[Cofoo[Anyx]].shouldBe(Anyx._deriving_cofoo)
  }

  it should "support AnyVal with type parameters" in {
    implicit val string: Cofoo[String] = null
    Valuezz._deriving_cofoo.shouldBe(null)
  }

  it should "support single parameter classes" in {
    implicitly[Cofoo[Van]].shouldBe(Van._deriving_cofoo)
    implicitly[Cobar[Van]].shouldBe(Van._deriving_b)
  }
}
