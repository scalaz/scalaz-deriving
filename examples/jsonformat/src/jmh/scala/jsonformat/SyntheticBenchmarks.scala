package jsonformat

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import io.circe.{ Error, Printer }
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._

// sbt clean 'jsonformat/jmh:run SyntheticBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 SyntheticBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

@deriving(Equal, Show, JsEncoder, JsDecoder)
case class Nested(n: Option[Nested])

@deriving(Equal, Show)
case class Nested2(n: Option[Nested2])

object Nested2 {
  implicit val customEncoder: JsEncoder[Nested2] = { o =>
    JsObject(
      o.n.fold(IList.empty[(String, JsValue)])(
        n => IList.single("n" -> n.toJson)
      )
    )
  }
  implicit val customDecoder: JsDecoder[Nested2] = { j =>
    j.asJsObject.flatMap { obj =>
      if (obj.fields.isEmpty) \/-(Nested2(None))
      else obj.getAs[Option[Nested2]]("n").map(Nested2(_))
    }
  }
}

@jmh.annotations.State(jmh.annotations.Scope.Thread)
@jmh.annotations.Warmup(iterations = 7, time = 1, timeUnit = TimeUnit.SECONDS)
@jmh.annotations.Measurement(
  iterations = 7,
  time = 1,
  timeUnit = TimeUnit.SECONDS
)
@jmh.annotations.Fork(
  value = 1,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseParallelGC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"
  )
)
@jmh.annotations.BenchmarkMode(Array(jmh.annotations.Mode.Throughput))
@jmh.annotations.OutputTimeUnit(TimeUnit.SECONDS)
class SyntheticBenchmarks {
  val circePrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true, reuseWriters = true)

  @jmh.annotations.Param(Array("1", "3", "10", "30", "100", "300"))
  var size: Int                       = 7
  var obj: Nested                     = _
  var obj2: Nested2                   = _
  var jsonString: String              = _
  var parsingErrorJsonString: String  = _
  var decodingErrorJsonString: String = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    obj = 1.to(size).foldLeft(Nested(None))((n, _) => Nested(Some(n)))
    obj2 = 1.to(size).foldLeft(Nested2(None))((n, _) => Nested2(Some(n)))
    jsonString = writeCirce()
    parsingErrorJsonString = jsonString.replace("{}", "xxx")
    decodingErrorJsonString = jsonString.replace("{}", "1")
    require(decodingErrorCirce().isLeft)
    require(decodingErrorScalazCustom().isLeft)
    require(decodingErrorScalazDeriving().isLeft)
    require(parsingErrorCirce().isLeft)
    require(parsingErrorScalazCustom().isLeft)
    require(parsingErrorScalazDeriving().isLeft)
    require(readCirce().right.get == obj)
    require(readScalazCustom().getOrElse(null) == obj2)
    require(readScalazDeriving().getOrElse(null) == obj)
    require(writeCirce() == jsonString)
    require(writeScalazCustom() == jsonString)
    // FIXME: java.lang.NoSuchMethodError: jsonformat.Nested$._deriving_jsencoder()Ljsonformat/JsEncoder;
    //require(writeScalazDeriving() == jsonString)
  }

  @jmh.annotations.Benchmark
  def decodingErrorCirce(): Either[Error, Nested] =
    decode[Nested](decodingErrorJsonString)

  @jmh.annotations.Benchmark
  def decodingErrorScalazCustom(): \/[String, Nested2] =
    JsParser(decodingErrorJsonString).flatMap(_.as[Nested2])

  @jmh.annotations.Benchmark
  def decodingErrorScalazDeriving(): \/[String, Nested] =
    JsParser(decodingErrorJsonString).flatMap(_.as[Nested])

  @jmh.annotations.Benchmark
  def parsingErrorCirce(): Either[Error, Nested] =
    decode[Nested](parsingErrorJsonString)

  @jmh.annotations.Benchmark
  def parsingErrorScalazCustom(): \/[String, Nested2] =
    JsParser(parsingErrorJsonString).flatMap(_.as[Nested2])

  @jmh.annotations.Benchmark
  def parsingErrorScalazDeriving(): \/[String, Nested] =
    JsParser(parsingErrorJsonString).flatMap(_.as[Nested])

  @jmh.annotations.Benchmark
  def readCirce(): Either[Error, Nested] = decode[Nested](jsonString)

  @jmh.annotations.Benchmark
  def readScalazCustom(): \/[String, Nested2] =
    JsParser(jsonString).flatMap(_.as[Nested2])

  @jmh.annotations.Benchmark
  def readScalazDeriving(): \/[String, Nested] =
    JsParser(jsonString).flatMap(_.as[Nested])

  @jmh.annotations.Benchmark
  def writeCirce(): String = circePrinter.pretty(obj.asJson)

  @jmh.annotations.Benchmark
  def writeScalazCustom(): String = CompactPrinter(obj2.toJson)

/* FIXME: java.lang.NoSuchMethodError: jsonformat.Nested$._deriving_jsencoder()Ljsonformat/JsEncoder;
  @jmh.annotations.Benchmark
  def writeScalazDeriving(): String = CompactPrinter(obj.toJson)
*/
}
