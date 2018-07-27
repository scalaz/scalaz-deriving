package jsonformat

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import io.circe.{ Error, Printer }
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._
import Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString

import scala.collection.immutable.Seq

// sbt clean 'jsonformat/jmh:run TwitterAPIBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 TwitterAPIBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

// reference for the format of tweets: https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html
@deriving(JsEncoder, JsDecoder)
case class Urls(
  url: String,
  expanded_url: String,
  display_url: String,
  indices: Seq[Int]
)

@deriving(JsEncoder, JsDecoder)
case class Url(urls: Seq[Urls])

@deriving(JsEncoder, JsDecoder)
case class UserEntities(url: Url, description: Url)

@deriving(JsEncoder, JsDecoder)
case class UserMentions(
  screen_name: String,
  name: String,
  id: Long,
  id_str: String,
  indices: Seq[Int]
)

@deriving(JsEncoder, JsDecoder)
case class User(
  id: Long,
  id_str: String,
  name: String,
  screen_name: String,
  location: String,
  description: String,
  url: String,
  entities: UserEntities,
  `protected`: Boolean,
  followers_count: Int,
  friends_count: Int,
  listed_count: Int,
  created_at: String,
  favourites_count: Int,
  utc_offset: Int,
  time_zone: String,
  geo_enabled: Boolean,
  verified: Boolean,
  statuses_count: Int,
  lang: String,
  contributors_enabled: Boolean,
  is_translator: Boolean,
  is_translation_enabled: Boolean,
  profile_background_color: String,
  profile_background_image_url: String,
  profile_background_image_url_https: String,
  profile_background_tile: Boolean,
  profile_image_url: String,
  profile_image_url_https: String,
  profile_banner_url: String,
  profile_link_color: String,
  profile_sidebar_border_color: String,
  profile_sidebar_fill_color: String,
  profile_text_color: String,
  profile_use_background_image: Boolean,
  has_extended_profile: Boolean,
  default_profile: Boolean,
  default_profile_image: Boolean,
  following: Boolean,
  follow_request_sent: Boolean,
  notifications: Boolean,
  translator_type: String
)

@deriving(JsEncoder, JsDecoder)
case class Entities(
  hashtags: Seq[String],
  symbols: Seq[String],
  user_mentions: Seq[UserMentions],
  urls: Seq[Urls]
)

@deriving(JsEncoder, JsDecoder)
case class RetweetedStatus(
  created_at: String,
  id: Long,
  id_str: String,
  text: String,
  truncated: Boolean,
  entities: Entities,
  source: String,
  in_reply_to_status_id: Option[String],
  in_reply_to_status_id_str: Option[String],
  in_reply_to_user_id: Option[String],
  in_reply_to_user_id_str: Option[String],
  in_reply_to_screen_name: Option[String],
  user: User,
  geo: Option[String],
  coordinates: Option[String],
  place: Option[String],
  contributors: Option[String],
  is_quote_status: Boolean,
  retweet_count: Int,
  favorite_count: Int,
  favorited: Boolean,
  retweeted: Boolean,
  possibly_sensitive: Boolean,
  lang: String
)

@deriving(JsEncoder, JsDecoder)
case class Tweet(
  created_at: String,
  id: Long,
  id_str: String,
  text: String,
  truncated: Boolean,
  entities: Entities,
  source: String,
  in_reply_to_status_id: Option[String],
  in_reply_to_status_id_str: Option[String],
  in_reply_to_user_id: Option[String],
  in_reply_to_user_id_str: Option[String],
  in_reply_to_screen_name: Option[String],
  user: User,
  geo: Option[String],
  coordinates: Option[String],
  place: Option[String],
  contributors: Option[String],
  retweeted_status: RetweetedStatus,
  is_quote_status: Boolean,
  retweet_count: Int,
  favorite_count: Int,
  favorited: Boolean,
  retweeted: Boolean,
  possibly_sensitive: Boolean,
  lang: String
)

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
class TwitterAPIBenchmarks {
  val circePrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true, reuseWriters = true)
  var jsonString: String  = _
  var jsonString2: String = _
  var obj: Seq[Tweet]     = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("twitter_api_response.json")
    jsonString2 = getResourceAsString("twitter_api_compact_response.json")
    obj = readCirce().right.get
    require(readCirce().right.get == obj)
    require(writeCirce() == jsonString2)
    require(readScalazDeriving().getOrElse(null) == obj)
    require(writeScalazDeriving() == jsonString2)
  }

  @jmh.annotations.Benchmark
  def readCirce(): Either[Error, Seq[Tweet]] = decode[Seq[Tweet]](jsonString)

  @jmh.annotations.Benchmark
  def readScalazDeriving(): \/[String, Seq[Tweet]] =
    JsParser(jsonString).flatMap(_.as[Seq[Tweet]])

  @jmh.annotations.Benchmark
  def writeCirce(): String = circePrinter.pretty(obj.asJson)

  @jmh.annotations.Benchmark
  def writeScalazDeriving(): String = CompactPrinter(obj.toJson)
}
