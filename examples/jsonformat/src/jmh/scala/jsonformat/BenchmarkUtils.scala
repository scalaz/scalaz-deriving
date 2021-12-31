// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

object BenchmarkUtils {
  def getResourceAsString(res: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(res)
    try {
      val baos     = new java.io.ByteArrayOutputStream()
      val data     = Array.ofDim[Byte](2048)
      var len: Int = 0
      def read(): Int = { len = is.read(data); len }
      while (read() != -1)
        baos.write(data, 0, len)
      baos.toString("UTF-8")
    } finally is.close()
  }
}
