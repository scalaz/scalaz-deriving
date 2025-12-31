/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.benchmarks

import jsonformat.*
import jsonformat.BenchmarkUtils.getResourceAsString
import jsonformat.JsDecoder.ops.*
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalaz.*
import scalaz.Scalaz.*

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 FieldLookupBenchmarks.*

final case class FieldLookup(
  id: Long
)

// when benching magnolia, we seen some strange perf patterns when looking up
// fields in the twitter model. My suspicion is that large case classes or
// backtick fields have a perf regression.
@State(Scope.Benchmark)
class FieldLookupBenchmarks {

  var user: s.User = _
  var baseline: FieldLookup = _

  @Setup
  def setup(): Unit = {
    val raw = getResourceAsString("twitter_api_response.json")
    val ast = JsParser(raw).getOrElse(null)
    user = ast.as[List[s.Tweet]].getOrElse(null).head.user
    baseline = FieldLookup(0)
  }

  @Benchmark
  def fieldAccessBaseline(): Long = baseline.id

  @Benchmark
  def elementAccessBaseline(): Long =
    baseline.productElement(0).asInstanceOf[Long]

  @Benchmark
  def fieldAccessPlain(): Long = user.id

  @Benchmark
  def elementAccessPlain(): Long = user.productElement(0).asInstanceOf[Long]

  @Benchmark
  def fieldAccessBacktick(): Boolean = user.`protected`

  @Benchmark
  def elementAccessBacktick(): Boolean =
    user.productElement(8).asInstanceOf[Boolean]

  @Benchmark
  def fieldAccessXl(): String = user.translator_type

  @Benchmark
  def elementAccessXl(): String = user.productElement(41).asInstanceOf[String]

  @Benchmark
  def indirectionAccessValXl(): String = translator_type_val(user)
  @Benchmark
  def indirectionAccessDefXl(): String = translator_type_def(user)
  @Benchmark
  def indirectionAccessDefValXl(): String =
    translator_type_abstract(user, translator_type_val)
  @Benchmark
  def indirectionAccessDefDefXl(): String =
    translator_type_abstract(user, translator_type_def)

  val translator_type_val = (u: s.User) => u.translator_type
  def translator_type_def(u: s.User) = u.translator_type
  def translator_type_abstract(u: s.User, f: s.User => String): String = f(u)

}
