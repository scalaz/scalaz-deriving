/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.benchmarks

import jsonformat._
import internal._
import org.openjdk.jmh.annotations.{ Benchmark, Param, Scope, Setup, State }
import scalaz._, Scalaz._

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 StringyMapBenchmarks.*

@State(Scope.Benchmark)
class StringyMapBenchmarks {
  @Param(Array("2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"))
  var size: Int = 0

  var data: IList[(String, String)]     = _
  var queries: IList[String]            = IList((size / 2).toString, "<not here>")
  var stringy_ilist: StringyMap[String] = _
  var stringy_java: StringyMap[String]  = _

  @Setup
  def setup(): Unit = {
    data = (0 |-> size).map(i => (i.toString, i.toString)).toIList
    stringy_ilist = createIList()
    stringy_java = createJava()
  }

  // @Benchmark
  def createIList(): StringyMap[String] = StringyIList(data)
  @Benchmark
  def createJava(): StringyMap[String]  = StringyJavaMap(data, 16)

  @Benchmark
  def lookupIList(): IList[Maybe[String]] = queries.map(stringy_ilist.get)
  @Benchmark
  def lookupJava(): IList[Maybe[String]]  = queries.map(stringy_java.get)

}

// jsonformat/jmh:run -i 1 -wi 1 -f1 -t2 -w1 -r1 StringyMapMoarBenchmarks.*
//
// jsonformat/jmh:run -i 1 -wi 1 -f1 -t2 -w1 -r1 StringyMapMoarBenchmarks.* -p numEntries=64 -p numQueries=64
@State(Scope.Benchmark)
class StringyMapMoarBenchmarks {
  @Param(Array("1", "2", "4", "8", "16"))
  var numEntries: Int = 0

  @Param(Array("1", "2", "3", "4", "5", "6"))
  var numQueries: Int = 0

  var data: IList[(String, String)] = _
  var queries: IList[String]        = _

  @Setup
  def setup(): Unit = {
    data = (0 |-> numEntries).map(i => (i.toString, i.toString)).toIList
    queries = (0 |-> numQueries).reverse.map(i => i.toString).toIList
  }

  @Benchmark
  def aggregateIList(): IList[Maybe[String]] = {
    val lookup = StringyIList(data)
    queries.map(lookup.get)
  }
  @Benchmark
  def aggregateJava(): IList[Maybe[String]]  = {
    val lookup = StringyJavaMap(data, numQueries)
    queries.map(lookup.get)
  }

}
