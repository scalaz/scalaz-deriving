/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package xmlformat

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import scalaz.*
import scalaz.Scalaz.*

trait EncoderTestsParent {
  this: AnyFlatSpecLike =>

  implicit class StringHelper(x: XTag) {
    def print: String = encode(x)
  }
  def encode(x: XTag): String

  def preamble: String = "<?xml version='1.0' encoding='UTF-8'?>\n"

  it should "support elements" in {
    XTag("foo", XString("wibble")).print
      .shouldBe(s"$preamble<foo>wibble</foo>")
  }

  it should "support nested elements and mixed content" in {
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
    ).print.shouldBe(s"""$preamble<root>
  <foo>wibble</foo>
  <foo>
    <bar>fish</bar>
    <bar>pants</bar>
    wobble1wobble2wobble3
  </foo>
  <foo>fish</foo>
</root>""")
  }

  it should "support empty tags" in {
    XTag(
      "foo",
      XChildren(
        IList(
          XTag("wobble", XChildren(IList.empty)),
          XTag("fish", XChildren(IList.empty))
        )
      )
    ).print.shouldBe(s"""$preamble<foo>
  <wobble/>
  <fish/>
</foo>""")
  }

  it should "support attributes" in {
    XTag(
      "foo",
      IList(XAttr("bar", XString("wobble"))),
      IList.empty,
      Maybe.just(XString("wibble"))
    ).print.shouldBe(
      s"""$preamble<foo bar="wobble">wibble</foo>"""
    )

    XTag(
      "foo",
      IList(XAttr("bar", XString(""))),
      IList.empty,
      Maybe.just(XString("wibble"))
    ).print.shouldBe(
      s"""$preamble<foo bar="">wibble</foo>"""
    )

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
    ).print
      .shouldBe(s"""$preamble<foo bar="BAR" baz="BAZ">
  <wobble/>
  <fish/>
</foo>""")
  }

  it should "support urlencoded attributes" in {
    XTag(
      "foo",
      IList(XAttr("bar", XString("wibble & wobble"))),
      IList.empty,
      Maybe.empty
    ).print.shouldBe(s"""$preamble<foo bar="wibble &amp; wobble"/>""")
  }

  it should "support encoded tag bodies" in {
    XTag("foo", XString("wibble & wobble")).print.shouldBe(
      s"$preamble<foo><![CDATA[wibble & wobble]]></foo>"
    )

    // requires URL encoding but not XML encoding
    XTag("foo", XString("%s")).print.shouldBe(s"$preamble<foo>%s</foo>")

    // nested
    XTag("foo", XString("<![CDATA[%s]]>")).print.shouldBe(
      s"$preamble<foo><![CDATA[<![CDATA[%s]]]]><![CDATA[>]]></foo>"
    )
  }

}
