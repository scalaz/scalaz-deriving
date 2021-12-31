// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package stax

import org.scalatest.matchers.should.Matchers._
import org.scalactic.source.Position

import scalaz._, Scalaz._
import org.scalatest.flatspec.AnyFlatSpec

class StaxDecoderTests extends AnyFlatSpec {
  import XTestUtils._

  implicit class StringHelper(txt: String) {
    def parsed(implicit P: Position): XTag =
      StaxDecoder.parse(txt).rightValue

    def as[A: XDecoder](implicit P: Position): A =
      parsed.as[A]

    def failsAs[T: XDecoder](implicit P: Position): String =
      StaxDecoder.parse(txt).leftValue
  }

  "Stax2 Decoder" should "support elements" in {
    val data     = "<foo>wibble</foo>"
    val expected = XTag("foo", XString("wibble"))

    data.parsed.shouldBe(expected)

    s"""<?xml version="1.0" encoding="UTF-8"?>$data""".parsed.shouldBe(expected)

    s"\n\n  $data\n ".parsed.shouldBe(expected)

    s"\n\n  $data\n <!-- comment -->".parsed.shouldBe(expected)

  }

  it should "support nested elements and mixed content" in {
    """<root>
       <foo>wibble</foo>
       <foo>
         wobble1
         <bar>
          fish
         </bar>
         wobble2
         <bar>
          pants
         </bar>
         wobble3
       </foo>
       <foo>fish</foo>
       </root>
    """.parsed
      .shouldBe(
        XTag(
          "root",
          XChildren(
            IList(
              XTag("foo", XString("wibble")),
              XTag(
                "foo",
                IList.empty,
                IList(
                  XTag("bar", XString("fish")),
                  XTag("bar", XString("pants"))
                ),
                XString("wobble1wobble2wobble3").just
              ),
              XTag("foo", XString("fish"))
            )
          )
        )
      )
  }

  it should "support empty tags" in {
    "<foo><wobble/><fish/></foo>".parsed
      .shouldBe(
        XTag(
          "foo",
          XChildren(
            IList(
              XTag("wobble", XChildren(IList.empty)),
              XTag("fish", XChildren(IList.empty))
            )
          )
        )
      )
  }

  it should "support attributes" in {
    "<foo bar='wobble'>wibble</foo>".parsed
      .shouldBe(
        XTag(
          "foo",
          IList(XAttr("bar", XString("wobble"))),
          IList.empty,
          Maybe.just(XString("wibble"))
        )
      )

    "<foo bar=''>wibble</foo>".parsed
      .shouldBe(
        XTag(
          "foo",
          IList(XAttr("bar", XString(""))),
          IList.empty,
          Maybe.just(XString("wibble"))
        )
      )

    "<foo bar='BAR' baz='BAZ'><wobble/><fish/></foo>".parsed
      .shouldBe(
        XTag(
          "foo",
          IList(
            XAttr("bar", XString("BAR")),
            XAttr("baz", XString("BAZ"))
          ),
          IList(
            XTag("wobble", XChildren(IList.empty)),
            XTag("fish", XChildren(IList.empty))
          ),
          Maybe.empty
        )
      )
  }

  it should "support urlencoded attributes" in {
    "<foo bar='wibble &amp; wobble'/>".parsed
      .shouldBe(
        XTag(
          "foo",
          IList(XAttr("bar", XString("wibble & wobble"))),
          IList.empty,
          Maybe.empty
        )
      )
  }

  it should "support encoded tag bodies" in {
    "<foo>wibble &amp; wobble</foo>".parsed.shouldBe(
      XTag("foo", XString("wibble & wobble"))
    )

    "<foo> <![CDATA[%s]]> </foo>".parsed.shouldBe(
      XTag("foo", XString("%s"))
    )

    // mixed
    """<foo>
         wibble &amp; wobble
         <![CDATA[%s]]>
         wobble &amp; wibble
      </foo>""".parsed.shouldBe(
      XTag("foo", XString("wibble& wobble%swobble& wibble"))
    )

    // nested
    "<foo><![CDATA[<![CDATA[%s]]]]><![CDATA[>]]></foo>".parsed.shouldBe(
      XTag("foo", XString("<![CDATA[%s]]>"))
    )
  }

}
