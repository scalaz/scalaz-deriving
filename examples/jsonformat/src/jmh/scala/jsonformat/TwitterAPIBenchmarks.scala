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

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 TwitterAPIBenchmarks.*
//
// see GoogleMapsAPIBenchmarks for profiling instructions

// reference for the format of tweets: https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html

package m {
  @deriving(JsEncoder, JsDecoder)
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )

  @deriving(JsEncoder, JsDecoder)
  case class Url(urls: IList[Urls])

  @deriving(JsEncoder, JsDecoder)
  case class UserEntities(url: Url, description: Url)

  @deriving(JsEncoder, JsDecoder)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
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
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
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
}

package s {
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )
  case class Url(urls: IList[Urls])
  case class UserEntities(url: Url, description: Url)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
  )
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
  case class Entities(
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
  )
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

  object Urls {
    implicit val encoder: JsEncoder[Urls] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Urls] = DerivedProductJsDecoder.gen
  }
  object Url {
    implicit val encoder: JsEncoder[Url] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Url] = DerivedProductJsDecoder.gen
  }
  object UserEntities {
    implicit val encoder: JsEncoder[UserEntities] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[UserEntities] = DerivedProductJsDecoder.gen
  }
  object UserMentions {
    implicit val encoder: JsEncoder[UserMentions] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[UserMentions] = DerivedProductJsDecoder.gen
  }
  object User {
    implicit val encoder: JsEncoder[User] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[User] = DerivedProductJsDecoder.gen
  }
  object Entities {
    implicit val encoder: JsEncoder[Entities] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Entities] = DerivedProductJsDecoder.gen
  }
  object RetweetedStatus {
    implicit val encoder: JsEncoder[RetweetedStatus] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[RetweetedStatus] =
      DerivedProductJsDecoder.gen
  }
  object Tweet {
    implicit val encoder: JsEncoder[Tweet] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Tweet] = DerivedProductJsDecoder.gen
  }
}

@State(Scope.Benchmark)
class TwitterAPIBenchmarks {
  var jsonString: String  = _
  var jsonString2: String = _
  var jsonString3: String = _
  var objm: List[m.Tweet] = _
  var objs: List[s.Tweet] = _
  var ast1: JsValue       = _
  var ast2: JsValue       = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("twitter_api_response.json")
    jsonString2 = getResourceAsString("twitter_api_compact_response.json")
    jsonString3 = getResourceAsString("twitter_api_error_response.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonString3).getOrElse(null)
    objm = decodeMagnolia().getOrElse(null)
    require(CompactPrinter(encodeMagnolia()) == jsonString2)
    require(decodeMagnoliaError.isLeft)

    objs = decodeShapeless().getOrElse(null)
    require(CompactPrinter(encodeShapeless()) == jsonString2)
    require(decodeShapelessError.isLeft)
  }

  @Benchmark
  def decodeMagnolia(): \/[String, List[m.Tweet]] =
    ast1.as[List[m.Tweet]]

  @Benchmark
  def decodeMagnoliaError(): \/[String, List[m.Tweet]] =
    ast2.as[List[m.Tweet]]

  @Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  @Benchmark
  def decodeShapeless(): \/[String, List[s.Tweet]] =
    ast1.as[List[s.Tweet]]

  @Benchmark
  def decodeShapelessError(): \/[String, List[s.Tweet]] =
    ast2.as[List[s.Tweet]]

  @Benchmark
  def encodeShapeless(): JsValue = objs.toJson

}
