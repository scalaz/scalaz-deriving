// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString
import org.openjdk.jmh.annotations.{ Benchmark, Scope, Setup, State }

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 GoogleMaps.*

// To enable the yourkit agent enable a profiling mode, e.g.:
//
// set jsonformat/neoJmhYourkit in Jmh := Seq("sampling", "onexit=snapshot")
// set jsonformat/neoJmhYourkit in Jmh := Seq("allocsampled", "onexit=snapshot")
//
// more options at https://www.yourkit.com/docs/java/help/startup_options.jsp
//
// When profiling only run one longer test at a time, e.g.
//
// jsonformat/jmh:run -i 1 -wi 0 -f0 -t1 -w0 -r10 GoogleMaps.*encodeMagnolia
//
// and look for the generated snapshot in YourKit (ignore the rest)

package m {
  @deriving(JsEncoder, JsDecoder)
  final case class Value(text: String, value: Int)

  @deriving(JsEncoder, JsDecoder)
  final case class Elements(distance: Value, duration: Value, status: String)

  @deriving(JsEncoder, JsDecoder)
  final case class Rows(elements: IList[Elements])

  @deriving(JsEncoder, JsDecoder)
  final case class DistanceMatrix(
    destination_addresses: IList[String],
    origin_addresses: IList[String],
    rows: IList[Rows],
    status: String
  )
}

package s {

  final case class Value(text: String, value: Int)
  final case class Elements(distance: Value, duration: Value, status: String)
  final case class Rows(elements: IList[Elements])
  final case class DistanceMatrix(
    destination_addresses: IList[String],
    origin_addresses: IList[String],
    rows: IList[Rows],
    status: String
  )

  object Value {
    implicit val encoder: JsEncoder[Value] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Value] = DerivedProductJsDecoder.gen
  }
  object Elements {
    implicit val encoder: JsEncoder[Elements] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Elements] = DerivedProductJsDecoder.gen
  }
  object Rows {
    implicit val encoder: JsEncoder[Rows] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Rows] = DerivedProductJsDecoder.gen
  }
  object DistanceMatrix {
    implicit val encoder: JsEncoder[DistanceMatrix] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[DistanceMatrix] =
      DerivedProductJsDecoder.gen
  }

}

@State(Scope.Benchmark)
class GoogleMapsAPIBenchmarks {
  var jsonString: String     = _
  var jsonString2: String    = _
  var jsonString3: String    = _
  var objm: m.DistanceMatrix = _
  var objs: s.DistanceMatrix = _
  var ast1, ast2: JsValue    = _

  @Setup
  def setup(): Unit = {
    //Distance Matrix API call for top-10 by population cities in US:
    //https://maps.googleapis.com/maps/api/distancematrix/json?origins=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose&destinations=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose
    jsonString = getResourceAsString("google_maps_api_response.json")
    jsonString2 = getResourceAsString("google_maps_api_compact_response.json")
    jsonString3 = getResourceAsString("google_maps_api_error_response.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonString3).getOrElse(null)

    objm = decodeMagnoliaSuccess().getOrElse(null)
    require(CompactPrinter(encodeMagnolia()) == jsonString2)
    require(decodeMagnoliaError().isLeft)

    objs = decodeShapelessSuccess().getOrElse(null)
    require(CompactPrinter(encodeShapeless()) == jsonString2)
    require(decodeShapelessError().isLeft)
  }

  @Benchmark
  def decodeMagnoliaSuccess(): \/[String, m.DistanceMatrix] =
    ast1.as[m.DistanceMatrix]

  @Benchmark
  def decodeMagnoliaError(): \/[String, m.DistanceMatrix] =
    ast2.as[m.DistanceMatrix]

  @Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  @Benchmark
  def decodeShapelessSuccess(): \/[String, s.DistanceMatrix] =
    ast1.as[s.DistanceMatrix]

  @Benchmark
  def decodeShapelessError(): \/[String, s.DistanceMatrix] =
    ast2.as[s.DistanceMatrix]

  @Benchmark
  def encodeShapeless(): JsValue = objs.toJson
}
