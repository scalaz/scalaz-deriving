// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package cord

import org.scalatest._
import org.scalatest.Matchers._
import CordEncoder.{ containsXmlEntities, replaceXmlEntities }

class CordEncoderTests extends FlatSpec with EncoderTestsParent {
  override def encode(t: XTag): String = CordEncoder.encode(t)

  "XML entities" should "be detected" in {
    containsXmlEntities("").shouldBe(false)

    containsXmlEntities("wibble\nwobble\n%").shouldBe(false)

    List("\"", "&", "'", "<", ">").foreach { e =>
      containsXmlEntities(e).shouldBe(true)

      containsXmlEntities("wibble " + e + " wobble").shouldBe(true)
      containsXmlEntities("\n\\" + e + "\n").shouldBe(true)
    }
  }

  it should "be replaced" in {
    replaceXmlEntities("").shouldBe("")

    replaceXmlEntities("wibble%").shouldBe("wibble%")

    replaceXmlEntities("""hello " world""").shouldBe("hello &quot; world")
    replaceXmlEntities("""hello & world""").shouldBe("hello &amp; world")
    replaceXmlEntities("""hello ' world""").shouldBe("hello &apos; world")
    replaceXmlEntities("""hello < world""").shouldBe("hello &lt; world")
    replaceXmlEntities("""hello > world""").shouldBe("hello &gt; world")

    replaceXmlEntities("""hello " & ' < > world""").shouldBe(
      "hello &quot; &amp; &apos; &lt; &gt; world"
    )
  }
}
