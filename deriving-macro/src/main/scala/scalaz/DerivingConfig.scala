// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.net.URL

import scala.Predef.{ wrapRefArray, ArrowAssoc }
import scala.collection.immutable.{ List, Map }
import scala.collection.JavaConverters._

private[scalaz] final case class DerivingConfig(targets: Map[String, String])
private[scalaz] object DerivingConfig extends BackCompat {
  private type Result[T] = Either[String, T]
  private type Stringy   = Map[String, String]

  private[scalaz] def targets(path: Option[String]): Result[Stringy] =
    for {
      d <- classpathTargets
      u <- path.map(user).getOrElse(EmptyTargets)
    } yield (d ++ u)
  private[this] val EmptyTargets: Result[Stringy] = Right(Map.empty)

  // cached to avoid hitting disk on every use of the macro.
  @volatile private[this] var cachedUserTargets: Map[String, Result[Stringy]] =
    Map.empty
  private[this] def user(path: String): Result[Stringy] =
    cachedUserTargets.get(path) match {
      case Some(got) => got
      case None =>
        val calculated = for {
          s <- readFile(path)
          c <- parseProperties(s)
        } yield c
        cachedUserTargets += path -> calculated
        calculated
    }

  private[this] lazy val classpathTargets: Result[Stringy] = {
    getClass.getClassLoader
      .getResources("deriving.conf")
      .asScala
      .toList
      .map { res =>
        for {
          s <- readResource(res)
          c <- parseProperties(s)
        } yield c
      }
      .fold(EmptyTargets) {
        // it's almost like we have a Monoid! Except, no, it's stdlib
        case (Right(m1), Right(m2)) => Right(m1 ++ m2)
        case (Left(e1), _)          => Left(e1)
        case (_, Left(e2))          => Left(e2)
      }
  }

  private[this] def parseProperties(config: String): Result[Stringy] =
    try {
      Right(
        config
          .split("\n")
          .toList
          .filterNot(_.isEmpty)
          .filterNot(_.startsWith("#"))
          .map(_.split("=").toList)
          .map {
            case List(from, to) => from.trim -> to.trim
            case other          =>
              // I'd have used Left with traverse, but this is stdlib...
              throw new java.lang.IllegalArgumentException(
                s"expected 2 parts but got ${other.size} in $other"
              )
          }
          .toMap
      )
    } catch {
      case t: java.lang.Throwable =>
        Left(t.getMessage)
    }

  private[this] def readFile(file: String): Either[String, String] =
    readInputStream(new java.io.FileInputStream(file))

  private[this] def readResource(resUrl: URL): Either[String, String] =
    readInputStream(resUrl.openStream())

  private[this] def readInputStream(
    is: java.io.InputStream
  ): Either[String, String] =
    try {
      val baos              = new java.io.ByteArrayOutputStream()
      val data              = scala.Array.ofDim[scala.Byte](2048)
      var len: scala.Int    = 0
      def read(): scala.Int = { len = is.read(data); len }
      while (read != -1) {
        baos.write(data, 0, len)
      }
      Right(new String(baos.toByteArray(), "UTF-8"))
    } catch {
      case t: java.lang.Throwable => Left(t.getMessage)
    } finally is.close()

}
