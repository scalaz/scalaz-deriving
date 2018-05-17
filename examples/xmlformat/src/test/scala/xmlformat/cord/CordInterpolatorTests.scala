// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.cord

import scalaz._, Scalaz._
import org.scalatest._

import org.scalatest._
import org.scalatest.Matchers._

class CordInterpolatorTests extends FlatSpec {

  "cord" should "interpolate trivial strings" in {
    cord"hello".toString.shouldBe("hello")
  }

  it should "interpolate Cord values" in {
    val a = Cord.stringToCord("hello")
    val b = Cord.stringToCord("world")
    cord"$a $b".toString.shouldBe("hello world")
  }

  it should "interpolate via Show instances" in {
    val a = "hello"
    val b = "world"
    cord"$a $b".toString.shouldBe("\"hello\" \"world\"")

    cord" $a $b ".toString.shouldBe(" \"hello\" \"world\" ")
  }

  it should "handle escape characters" in {
    val a = "hello"
    val b = "world"
    cord"$a\n$b".toString.shouldBe("\"hello\"\n\"world\"")

    cord"""
$a

$b
""".toString.shouldBe("\n\"hello\"\n\n\"world\"\n")
  }

}
