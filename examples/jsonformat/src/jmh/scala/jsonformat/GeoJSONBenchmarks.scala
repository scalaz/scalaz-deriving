// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import fommil.DerivedEqual
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsDecoder.fail
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._
import scalaz.annotation.deriving
import jsonformat.BenchmarkUtils.getResourceAsString
import org.openjdk.jmh.annotations.{ Benchmark, Scope, Setup, State }

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 GeoJSONBenchmarks.*
//
// see GoogleMapsAPIBenchmarks for profiling instructions

object orphans {
  implicit val tupleDblDblEncoder: JsEncoder[(Double, Double)] = {
    case (lat, lon) => JsArray(IList(lat.toJson, lon.toJson))
  }
  implicit val tupleDblDblDecoder: JsDecoder[(Double, Double)] =
    JsDecoder[IList[Double]].emap {
      case ICons(car, ICons(ccar, INil())) => \/-((car, ccar))
      case other                           =>
        JsDecoder.fail("2 numbers", JsArray(other.map(JsDouble(_))))
    }
}
import orphans._

package m {

  @deriving(JsEncoder, JsDecoder)
  sealed abstract class Geometry
  final case class Point(coordinates: (Double, Double)) extends Geometry
  final case class MultiPoint(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class LineString(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class MultiLineString(
    coordinates: IList[IList[(Double, Double)]]
  )                                                     extends Geometry
  final case class Polygon(coordinates: IList[IList[(Double, Double)]])
      extends Geometry
  final case class MultiPolygon(
    coordinates: IList[IList[IList[(Double, Double)]]]
  )                                                     extends Geometry
  final case class GeometryCollection(geometries: IList[Geometry])
      extends Geometry

  @deriving(JsEncoder, JsDecoder)
  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON

  object Geometry           {
    implicit val equal: Equal[Geometry] = MagnoliaEqual.gen
  }
  object Point              {
    implicit val equal: Equal[Point] = MagnoliaEqual.gen
  }
  object MultiPoint         {
    implicit val equal: Equal[MultiPoint] = MagnoliaEqual.gen
  }
  object LineString         {
    implicit val equal: Equal[LineString] = MagnoliaEqual.gen
  }
  object MultiLineString    {
    implicit val equal: Equal[MultiLineString] = MagnoliaEqual.gen
  }
  object Polygon            {
    implicit val equal: Equal[Polygon] = MagnoliaEqual.gen
  }
  object MultiPolygon       {
    implicit val equal: Equal[MultiPolygon] = MagnoliaEqual.gen
  }
  object GeometryCollection {
    implicit val equal: Equal[GeometryCollection] = MagnoliaEqual.gen
  }
  object GeoJSON            {
    implicit val equal: Equal[GeoJSON] = MagnoliaEqual.gen
  }
  object Feature            {
    implicit val equal: Equal[Feature] = MagnoliaEqual.gen
  }
  object FeatureCollection  {
    implicit val equal: Equal[FeatureCollection] = MagnoliaEqual.gen
  }

}

package s {

  sealed abstract class Geometry
  final case class Point(coordinates: (Double, Double)) extends Geometry
  final case class MultiPoint(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class LineString(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class MultiLineString(
    coordinates: IList[IList[(Double, Double)]]
  )                                                     extends Geometry
  final case class Polygon(coordinates: IList[IList[(Double, Double)]])
      extends Geometry
  final case class MultiPolygon(
    coordinates: IList[IList[IList[(Double, Double)]]]
  )                                                     extends Geometry
  final case class GeometryCollection(geometries: IList[Geometry])
      extends Geometry

  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON

  object Geometry           {
    implicit val encoder: JsEncoder[Geometry] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Geometry] = DerivedCoproductJsDecoder.gen
    implicit val equal: Equal[Geometry]       = DerivedEqual.gen
  }
  object Point              {
    implicit val encoder: JsEncoder[Point] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Point] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Point]       = DerivedEqual.gen
  }
  object MultiPoint         {
    implicit val encoder: JsEncoder[MultiPoint] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiPoint] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[MultiPoint]       = DerivedEqual.gen
  }
  object LineString         {
    implicit val encoder: JsEncoder[LineString] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[LineString] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[LineString]       = DerivedEqual.gen
  }
  object MultiLineString    {
    implicit val encoder: JsEncoder[MultiLineString] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiLineString] =
      DerivedProductJsDecoder.gen
    implicit val equal: Equal[MultiLineString]       = DerivedEqual.gen
  }
  object Polygon            {
    implicit val encoder: JsEncoder[Polygon] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Polygon] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Polygon]       = DerivedEqual.gen

  }
  object MultiPolygon       {
    implicit val encoder: JsEncoder[MultiPolygon] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiPolygon] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[MultiPolygon]       = DerivedEqual.gen

  }
  object GeometryCollection {
    implicit val encoder: JsEncoder[GeometryCollection] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[GeometryCollection] =
      DerivedProductJsDecoder.gen
    implicit val equal: Equal[GeometryCollection]       = DerivedEqual.gen

  }

  object GeoJSON           {
    implicit val encoder: JsEncoder[GeoJSON] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[GeoJSON] = DerivedCoproductJsDecoder.gen
    implicit val equal: Equal[GeoJSON]       = DerivedEqual.gen
  }
  object Feature           {
    implicit val encoder: JsEncoder[Feature] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Feature] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Feature]       = DerivedEqual.gen
  }
  object FeatureCollection {
    implicit val encoder: JsEncoder[FeatureCollection] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[FeatureCollection] =
      DerivedProductJsDecoder.gen
    implicit val equal: Equal[FeatureCollection]       = DerivedEqual.gen
  }

}

package z {

  @deriving(JsDecoder)
  sealed abstract class Geometry
  final case class Point(coordinates: (Double, Double)) extends Geometry
  final case class MultiPoint(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class LineString(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class MultiLineString(
    coordinates: IList[IList[(Double, Double)]]
  )                                                     extends Geometry
  final case class Polygon(coordinates: IList[IList[(Double, Double)]])
      extends Geometry
  final case class MultiPolygon(
    coordinates: IList[IList[IList[(Double, Double)]]]
  )                                                     extends Geometry
  final case class GeometryCollection(geometries: IList[Geometry])
      extends Geometry

  @deriving(JsDecoder)
  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON

  object Geometry           {
    implicit val equal: Equal[Geometry] = Deriving.gen[Equal, Geometry]
  }
  object Point              {
    implicit val equal: Equal[Point] = Deriving.gen[Equal, Point]
  }
  object MultiPoint         {
    implicit val equal: Equal[MultiPoint] = Deriving.gen[Equal, MultiPoint]
  }
  object LineString         {
    implicit val equal: Equal[LineString] = Deriving.gen[Equal, LineString]
  }
  object MultiLineString    {
    implicit val equal: Equal[MultiLineString] =
      Deriving.gen[Equal, MultiLineString]
  }
  object Polygon            {
    implicit val equal: Equal[Polygon] = Deriving.gen[Equal, Polygon]
  }
  object MultiPolygon       {
    implicit val equal: Equal[MultiPolygon] = Deriving.gen[Equal, MultiPolygon]
  }
  object GeometryCollection {
    implicit val equal: Equal[GeometryCollection] =
      Deriving.gen[Equal, GeometryCollection]
  }
  object GeoJSON            {
    implicit val equal: Equal[GeoJSON] = Deriving.gen[Equal, GeoJSON]
  }
  object Feature            {
    implicit val equal: Equal[Feature] = Deriving.gen[Equal, Feature]
  }
  object FeatureCollection  {
    implicit val equal: Equal[FeatureCollection] =
      Deriving.gen[Equal, FeatureCollection]
  }

}

package h {

  // this is kind of cheating, but it's to prove a point that manual instances
  // can be orders of magnitude faster. In this case we're using a manual
  // instance of a custom case class that avoids the boxing of the doubles seen
  // in the other implementations, and bypassing straight to natural equality.

  final case class Coords(x: Double, y: Double)
  object Coords {
    implicit val encoder: JsEncoder[Coords] = a =>
      JsArray(IList(JsDouble(a.x), JsDouble(a.y)))
    implicit val decoder: JsDecoder[Coords] = {
      case JsArray(ICons(JsDouble(x), ICons(JsDouble(y), INil()))) =>
        Coords(x, y).right
      case other                                                   => fail("JsArray with two numbers", other)
    }
    implicit val equal: Equal[Coords]       = (a1, a2) => a1.x == a2.x && a1.y == a2.y
  }

  sealed abstract class Geometry
  final case class Point(coordinates: Coords)                 extends Geometry
  final case class MultiPoint(coordinates: IList[Coords])     extends Geometry
  final case class LineString(coordinates: IList[Coords])     extends Geometry
  final case class MultiLineString(
    coordinates: IList[IList[Coords]]
  )                                                           extends Geometry
  final case class Polygon(coordinates: IList[IList[Coords]]) extends Geometry
  final case class MultiPolygon(
    coordinates: IList[IList[IList[Coords]]]
  )                                                           extends Geometry
  final case class GeometryCollection(geometries: IList[Geometry])
      extends Geometry

  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON

  object Geometry {
    private def list[A: JsEncoder](
      field: String,
      as: IList[A]
    ): IList[(String, JsValue)]               =
      if (as.isEmpty) IList.empty
      else field -> as.toJson :: IList.empty

    implicit val encoder: JsEncoder[Geometry] = {
      case Point(c)               =>
        JsObject(
          "type"          -> JsString("Point") ::
            "coordinates" -> c.toJson ::
            IList.empty
        )
      case MultiPoint(c)          =>
        JsObject(
          "type" -> JsString("MultiPoint") ::
            list("coordinates", c)
        )
      case LineString(c)          =>
        JsObject(
          "type" -> JsString("LineString") ::
            list("coordinates", c)
        )
      case MultiLineString(c)     =>
        JsObject(
          "type" -> JsString("MultiLineString") ::
            list("coordinates", c)
        )
      case Polygon(c)             =>
        JsObject(
          "type" -> JsString("Polygon") ::
            list("coordinates", c)
        )
      case MultiPolygon(c)        =>
        JsObject(
          "type" -> JsString("MultiPolygon") ::
            list("coordinates", c)
        )
      case GeometryCollection(gs) =>
        JsObject(
          "type" -> JsString("GeometryCollection") ::
            list("geometries", gs)
        )
    }
    implicit val decoder: JsDecoder[Geometry] = JsDecoder.obj(2) { j =>
      j.get("type").flatMap {
        case JsString("Point")              =>
          j.getAs[Coords]("coordinates").map(Point(_))
        case JsString("MultiPoint")         =>
          j.getNullable[IList[Coords]]("coordinates").map(MultiPoint(_))
        case JsString("LineString")         =>
          j.getNullable[IList[Coords]]("coordinates").map(LineString(_))
        case JsString("MultiLineString")    =>
          j.getNullable[IList[IList[Coords]]]("coordinates")
            .map(MultiLineString(_))
        case JsString("Polygon")            =>
          j.getNullable[IList[IList[Coords]]]("coordinates").map(Polygon(_))
        case JsString("MultiPolygon")       =>
          j.getNullable[IList[IList[IList[Coords]]]]("coordinates")
            .map(MultiPolygon(_))
        case JsString("GeometryCollection") =>
          j.getNullable[IList[Geometry]]("geometries")
            .map(GeometryCollection(_))
        case other                          => fail("valid type field", other)
      }
    }
    implicit val equal: Equal[Geometry]       = (a1, a2) =>
      a1 match {
        case Point(c1)               =>
          a2 match {
            case Point(c2) => c1 === c2
            case _         => false
          }
        case MultiPoint(c1)          =>
          a2 match {
            case MultiPoint(c2) => c1 === c2
            case _              => false
          }
        case LineString(c1)          =>
          a2 match {
            case LineString(c2) => c1 === c2
            case _              => false
          }
        case MultiLineString(c1)     =>
          a2 match {
            case MultiLineString(c2) => c1 === c2
            case _                   => false
          }
        case Polygon(c1)             =>
          a2 match {
            case Polygon(c2) => c1 === c2
            case _           => false
          }
        case MultiPolygon(c1)        =>
          a2 match {
            case MultiPolygon(c2) => c1 === c2
            case _                => false
          }
        case GeometryCollection(gs1) =>
          a2 match {
            case GeometryCollection(gs2) => gs1 === gs2
            case _                       => false
          }
      }

  }

  object GeoJSON {
    private def list[A: JsEncoder](
      field: String,
      as: IList[A]
    ): IList[(String, JsValue)]              =
      if (as.isEmpty) IList.empty
      else field -> as.toJson :: IList.empty

    private def dict[A: JsEncoder](
      field: String,
      as: Map[String, A]
    ): IList[(String, JsValue)]              =
      if (as.isEmpty) IList.empty
      else field -> as.toJson :: IList.empty

    implicit val encoder: JsEncoder[GeoJSON] = {
      case Feature(props, geo)         =>
        JsObject(
          "type"       -> JsString("Feature") ::
            dict("properties", props) :::
            "geometry" -> geo.toJson ::
            IList.empty
        )
      case FeatureCollection(features) =>
        JsObject(
          "type" -> JsString("FeatureCollection") ::
            list("features", features)
        )
    }
    implicit val decoder: JsDecoder[GeoJSON] = JsDecoder.obj(3) { j =>
      j.get("type").flatMap {
        case JsString("Feature")           =>
          for {
            props <- j.getNullable[Map[String, String]]("properties")
            geo   <- j.getAs[Geometry]("geometry")
          } yield Feature(props, geo)
        case JsString("FeatureCollection") =>
          j.getNullable[IList[GeoJSON]]("features").map(FeatureCollection(_))
        case other                         => fail("valid type field", other)
      }
    }
    implicit val equal: Equal[GeoJSON]       = (a1, a2) =>
      a1 match {
        case Feature(props1, geo1)        =>
          a2 match {
            case Feature(props2, geo2) => props1 === props2 && geo1 === geo2
            case _                     => false
          }
        case FeatureCollection(features1) =>
          a2 match {
            case FeatureCollection(features2) => features1 === features2
            case _                            => false
          }
      }
  }

}

@State(Scope.Benchmark)
class GeoJSONBenchmarks {
  var jsonString: String              = _
  var jsonString2: String             = _
  var jsonStringErr: String           = _
  var objm, objm_, objm__ : m.GeoJSON = _
  var objs, objs_, objs__ : s.GeoJSON = _
  var objz, objz_, objz__ : z.GeoJSON = _
  var objh, objh_, objh__ : h.GeoJSON = _
  var ast1, ast2, astErr: JsValue     = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("che.geo.json")
    jsonString2 = getResourceAsString("che-2.geo.json")
    jsonStringErr = getResourceAsString("che-err.geo.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonString2).getOrElse(null)
    astErr = JsParser(jsonStringErr).getOrElse(null)
    objm = decodeMagnoliaSuccess().getOrElse(null)
    require(decodeMagnoliaError.isLeft)
    require(CompactPrinter(encodeMagnolia()) == jsonString)
    objs = decodeShapelessSuccess().getOrElse(null)
    require(decodeShapelessError.isLeft)
    require(CompactPrinter(encodeShapeless()) == jsonString)
    objh = decodeManualSuccess().getOrElse(null)
    require(decodeManualError.isLeft)
    require(CompactPrinter(encodeManual()) == jsonString)

    objz = decodeScalazSuccess().getOrElse(null)
    // not reusing the same objects to avoid instance identity
    objm_ = decodeMagnoliaSuccess().getOrElse(null)
    objs_ = decodeShapelessSuccess().getOrElse(null)
    objz_ = decodeScalazSuccess().getOrElse(null)
    objh_ = decodeManualSuccess().getOrElse(null)
    objm__ = ast2.as[m.GeoJSON].getOrElse(null)
    objs__ = ast2.as[s.GeoJSON].getOrElse(null)
    objz__ = ast2.as[z.GeoJSON].getOrElse(null)
    objh__ = ast2.as[h.GeoJSON].getOrElse(null)
  }

  @Benchmark
  def decodeMagnoliaSuccess(): \/[String, m.GeoJSON] =
    ast1.as[m.GeoJSON]

  @Benchmark
  def decodeMagnoliaError(): String \/ m.GeoJSON =
    astErr.as[m.GeoJSON]

  @Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  @Benchmark
  def decodeManualSuccess(): \/[String, h.GeoJSON] =
    ast1.as[h.GeoJSON]

  @Benchmark
  def decodeManualError(): String \/ h.GeoJSON =
    astErr.as[h.GeoJSON]

  @Benchmark
  def encodeManual(): JsValue = objh.toJson

  def decodeScalazSuccess(): \/[String, z.GeoJSON] =
    ast1.as[z.GeoJSON]

  @Benchmark
  def decodeShapelessSuccess(): \/[String, s.GeoJSON] =
    ast1.as[s.GeoJSON]

  @Benchmark
  def decodeShapelessError(): String \/ s.GeoJSON =
    astErr.as[s.GeoJSON]

  @Benchmark
  def encodeShapeless(): JsValue = objs.toJson

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
