// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import fommil.DerivedEqual
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._
import scalaz.annotation.deriving
import jsonformat.BenchmarkUtils.getResourceAsString
import org.openjdk.jmh.annotations.{ Benchmark, Scope, Setup, State }

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 GoogleMaps.*

// To enable the yourkit agent enable a profiling mode, e.g.:
//
// set jsonformat/neoJmhYourkit in Jmh := Seq("sampling")
// set jsonformat/neoJmhYourkit in Jmh := Seq("allocsampled")
//
// more options at https://www.yourkit.com/docs/java/help/startup_options.jsp
//
// When profiling only run one longer test at a time, e.g.
//
// jsonformat/jmh:run -i 1 -wi 0 -f0 -t1 -w0 -r10 GoogleMaps.*encodeMagnolia
//
// and look for the generated snapshot in YourKit (ignore the rest)
//
// Also try the async profiler, e.g.
//
//  jsonformat/jmh:run -i 1 -wi 0 -f1 -t1 -w0 -r10 -prof jmh.extras.Async GoogleMaps.*encodeMagnolia
//  jsonformat/jmh:run -i 1 -wi 0 -f1 -t1 -w0 -r10 -prof jmh.extras.Async:event=alloc GoogleMaps.*encodeMagnolia
//
// which may require kernel permissions:
//
//   echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid
//   echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
//
// and needs these projects installed, with these variables:
//
// export ASYNC_PROFILER_DIR=$HOME/Projects/async-profiler
// export FLAME_GRAPH_DIR=$HOME/Projects/FlameGraph
//
// http://malaw.ski/2017/12/10/automatic-flamegraph-generation-from-jmh-benchmarks-using-sbt-jmh-extras-plain-java-too/
// (note you need to type `make` in the async-profiler directory)
//
// to use allocation profiling, you need debugging symbols in your jvm. e.g. use
// the Zulu Java distribution.

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

  object Value          {
    implicit val equal: Equal[Value] = MagnoliaEqual.gen
  }
  object Elements       {
    implicit val equal: Equal[Elements] = MagnoliaEqual.gen
  }
  object Rows           {
    implicit val equal: Equal[Rows] = MagnoliaEqual.gen
  }
  object DistanceMatrix {
    implicit val equal: Equal[DistanceMatrix] = MagnoliaEqual.gen
  }
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

  object Value          {
    implicit val encoder: JsEncoder[Value] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Value] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Value]       = DerivedEqual.gen
  }
  object Elements       {
    implicit val encoder: JsEncoder[Elements] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Elements] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Elements]       = DerivedEqual.gen
  }
  object Rows           {
    implicit val encoder: JsEncoder[Rows] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Rows] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Rows]       = DerivedEqual.gen
  }
  object DistanceMatrix {
    implicit val encoder: JsEncoder[DistanceMatrix] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[DistanceMatrix] =
      DerivedProductJsDecoder.gen
    implicit val equal: Equal[DistanceMatrix]       = DerivedEqual.gen
  }

}

package z {
  @deriving(JsDecoder)
  final case class Value(text: String, value: Int)

  @deriving(JsDecoder)
  final case class Elements(distance: Value, duration: Value, status: String)

  @deriving(JsDecoder)
  final case class Rows(elements: IList[Elements])

  @deriving(JsDecoder)
  final case class DistanceMatrix(
    destination_addresses: IList[String],
    origin_addresses: IList[String],
    rows: IList[Rows],
    status: String
  )

  object Value          {
    implicit val equal: Equal[Value] = Deriving.gen[Equal, Value]
  }
  object Elements       {
    implicit val equal: Equal[Elements] = Deriving.gen[Equal, Elements]
  }
  object Rows           {
    implicit val equal: Equal[Rows] = Deriving.gen[Equal, Rows]
  }
  object DistanceMatrix {
    implicit val equal: Equal[DistanceMatrix] =
      Deriving.gen[Equal, DistanceMatrix]
  }

}

package h {

  final case class Value(text: String, value: Int)
  final case class Elements(distance: Value, duration: Value, status: String)
  final case class Rows(elements: IList[Elements])
  final case class DistanceMatrix(
    destination_addresses: IList[String],
    origin_addresses: IList[String],
    rows: IList[Rows],
    status: String
  )

  object Value          {
    implicit val encoder: JsEncoder[Value] = a =>
      JsObject(
        "text"    -> a.text.toJson ::
          "value" -> a.value.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Value] = JsDecoder.obj(2) { j =>
      for {
        text  <- j.getAs[String]("text")
        value <- j.getAs[Int]("value")
      } yield Value(text, value)
    }
    implicit val equal: Equal[Value]       = (a1, a2) =>
      a1.text === a2.text && a1.value === a2.value
  }
  object Elements       {
    implicit val encoder: JsEncoder[Elements] = a =>
      JsObject(
        "distance"   -> a.distance.toJson ::
          "duration" -> a.duration.toJson ::
          "status"   -> a.status.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Elements] = JsDecoder.obj(3) { j =>
      for {
        distance <- j.getAs[Value]("distance")
        duration <- j.getAs[Value]("duration")
        status   <- j.getAs[String]("status")
      } yield Elements(distance, duration, status)
    }
    implicit val equal: Equal[Elements]       = (a1, a2) =>
      a1.distance === a2.distance &&
        a1.duration === a2.duration &&
        a1.status === a2.status
  }
  object Rows           {
    private def list[A: JsEncoder](
      field: String,
      as: IList[A]
    ): IList[(String, JsValue)] =
      if (as.isEmpty) IList.empty
      else field -> as.toJson :: IList.empty

    implicit val encoder: JsEncoder[Rows] = a =>
      JsObject(
        list("elements", a.elements)
      )
    implicit val decoder: JsDecoder[Rows] = JsDecoder.obj(1) { j =>
      j.getNullable[IList[Elements]]("elements").map(Rows(_))
    }
    implicit val equal: Equal[Rows]       = (a1, a2) => a1.elements === a2.elements
  }
  object DistanceMatrix {
    private def list[A: JsEncoder](
      field: String,
      as: IList[A]
    ): IList[(String, JsValue)] =
      if (as.isEmpty) IList.empty
      else field -> as.toJson :: IList.empty

    implicit val encoder: JsEncoder[DistanceMatrix] = a =>
      JsObject(
        list("destination_addresses", a.destination_addresses) :::
          list("origin_addresses", a.origin_addresses) :::
          list("rows", a.rows) :::
          "status" -> a.status.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[DistanceMatrix] = JsDecoder.obj(4) { j =>
      for {
        dest   <- j.getNullable[IList[String]]("destination_addresses")
        origin <- j.getNullable[IList[String]]("origin_addresses")
        rows   <- j.getNullable[IList[Rows]]("rows")
        status <- j.getAs[String]("status")
      } yield DistanceMatrix(dest, origin, rows, status)
    }
    implicit val equal: Equal[DistanceMatrix]       = (a1, a2) =>
      a1.destination_addresses === a2.destination_addresses &&
        a1.origin_addresses === a2.origin_addresses &&
        a1.rows === a2.rows &&
        a1.status === a2.status
  }

}
@State(Scope.Benchmark)
class GoogleMapsAPIBenchmarks {
  var jsonString: String                     = _
  var jsonString2: String                    = _
  var jsonString3: String                    = _
  var objm, objm_, objm__ : m.DistanceMatrix = _
  var objs, objs_, objs__ : s.DistanceMatrix = _
  var objz, objz_, objz__ : z.DistanceMatrix = _
  var objh, objh_, objh__ : h.DistanceMatrix = _
  var ast1, ast2: JsValue                    = _

  @Setup
  def setup(): Unit = {
    // Distance Matrix API call for top-10 by population cities in US:
    // https://maps.googleapis.com/maps/api/distancematrix/json?origins=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose&destinations=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose
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

    objh = decodeManualSuccess().getOrElse(null)
    require(CompactPrinter(encodeManual()) == jsonString2)
    require(decodeManualError().isLeft)

    objz = decodeScalazSuccess().getOrElse(null)
    // not reusing the same objects to avoid instance identity
    objm_ = decodeMagnoliaSuccess().getOrElse(null)
    objs_ = decodeShapelessSuccess().getOrElse(null)
    objz_ = decodeScalazSuccess().getOrElse(null)
    objh_ = decodeManualSuccess().getOrElse(null)
    objm__ = decodeMagnoliaSuccess().getOrElse(null).copy(status = "z")
    objs__ = decodeShapelessSuccess().getOrElse(null).copy(status = "z")
    objz__ = decodeScalazSuccess().getOrElse(null).copy(status = "z")
    objh__ = decodeManualSuccess().getOrElse(null).copy(status = "z")
  }

  @Benchmark
  def decodeMagnoliaSuccess(): \/[String, m.DistanceMatrix] =
    ast1.as[m.DistanceMatrix]

  @Benchmark
  def decodeMagnoliaError(): \/[String, m.DistanceMatrix] =
    ast2.as[m.DistanceMatrix]

  def decodeScalazSuccess(): \/[String, z.DistanceMatrix] =
    ast1.as[z.DistanceMatrix]

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

  @Benchmark
  def decodeManualSuccess(): \/[String, h.DistanceMatrix] =
    ast1.as[h.DistanceMatrix]

  @Benchmark
  def decodeManualError(): \/[String, h.DistanceMatrix] =
    ast2.as[h.DistanceMatrix]

  @Benchmark
  def encodeManual(): JsValue = objh.toJson

  @Benchmark
  def equalScalazTrue(): Boolean = objz === objz_

  @Benchmark
  def equalScalazFalse(): Boolean = objz === objz__

  @Benchmark
  def equalMagnoliaTrue(): Boolean = objm === objm_

  @Benchmark
  def equalMagnoliaFalse(): Boolean = objm === objm__

  @Benchmark
  def equalShapelessTrue(): Boolean = objs === objs_

  @Benchmark
  def equalShapelessFalse(): Boolean = objs === objs__

  @Benchmark
  def equalManualTrue(): Boolean = objh === objh_

  @Benchmark
  def equalManualFalse(): Boolean = objh === objh__

}
