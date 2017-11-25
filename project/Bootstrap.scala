// https://raw.githubusercontent.com/scalameta/scalafmt/v0.6.6/bootstrap/src/main/scala/org/scalafmt/bootstrap/Bootstrap.scala
package org.scalafmt.bootstrap

import java.io._
import java.net.URLClassLoader

import coursier._

import scala.collection.immutable.Nil
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.{ Failure, Success }
import scalaz._
import scalaz.concurrent.Task

object `package` {
  type ScalafmtCli = {
    def main(args: Array[String]): Unit
    def main(args: Array[String],
             in: InputStream,
             out: PrintStream,
             err: PrintStream,
             workingDirectory: String): Unit
  }
}

class FetchError(errors: Seq[(Dependency, Seq[String])])
    extends Exception(errors.toString())

sealed abstract class ScalafmtBootstrap(val cli: ScalafmtCli) {

  def main(array: Seq[String]): Unit =
    cli.main(array.to[Array])

  def format(code: String): String = {
    val baos             = new ByteArrayOutputStream()
    val workingDirectory = new File("").getAbsolutePath
    val ctx = cli.main(
      Array("--stdin"),
      new ByteArrayInputStream(code.getBytes()),
      new PrintStream(baos),
      new PrintStream(new ByteArrayOutputStream()),
      workingDirectory
    )
    new String(baos.toByteArray)
  }
}

object ScalafmtBootstrap {
  private val cliCache =
    mutable.Map.empty[String, Either[Throwable, ScalafmtBootstrap]]

  def fromVersion(version: String): Either[Throwable, ScalafmtBootstrap] =
    synchronized {
      cliCache.getOrElseUpdate(version, fromVersionUncached(version))
    }

  def fromVersionUncached(
    version: String
  ): Either[Throwable, ScalafmtBootstrap] = synchronized {
    val start =
      Resolution(
        Set(
          Dependency(Module("com.geirsson", "scalafmt-cli_2.12"), version)
        )
      )

    val repositories = MavenRepository(
      "https://dl.bintray.com/scalameta/maven/"
    ) :: MavenRepository("https://repo1.maven.org/maven2") :: Nil

    val fetch = Fetch.from(
      repositories,
      Cache.fetch(cachePolicy = CachePolicy.default.head),
      CachePolicy.default.tail.map(p => Cache.fetch(cachePolicy = p)): _*
    )
    val resolved = start.process.run(fetch).unsafePerformSync
    resolved.metadataErrors.foreach { err =>
      throw new RuntimeException(s"failed to resolve $err")
    }

    val jars = Task
      .gatherUnordered(resolved.artifacts.map { artifact =>
        def fetch(p: CachePolicy) =
          coursier.Cache.file(artifact, cachePolicy = p)

        (fetch(CachePolicy.default.head) /: CachePolicy.default.tail)(
          _ orElse fetch(_)
        ).run
      })
      .unsafePerformSync
      .flatMap {
        case -\/(err)                                    => throw new RuntimeException(err.message)
        case \/-(file) if !file.getName.endsWith(".jar") => None
        case \/-(file)                                   => Some(file)
      }

    val urls                    = jars.map(_.toURI.toURL)
    val classLoader             = new URLClassLoader(urls.toArray, null)
    val reflectiveDynamicAccess = new ReflectiveDynamicAccess(classLoader)
    val loadedClass =
      reflectiveDynamicAccess
        .createInstanceFor[ScalafmtCli]("org.scalafmt.cli.Cli$", Nil)
    loadedClass match {
      case Success(cli) => Right(new ScalafmtBootstrap(cli) {})
      case Failure(e)   => Left(e)
    }
  }

}

import java.lang.reflect.InvocationTargetException

import scala.collection.immutable
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Original:
 * https://github.com/akka/akka/blob/master/akka-actor/src/main/scala/akka/actor/ReflectiveDynamicAccess.scala
 *
 */
class ReflectiveDynamicAccess(val classLoader: ClassLoader) {

  def getClassFor[T: ClassTag](fqcn: String): Try[Class[_ <: T]] =
    Try[Class[_ <: T]]({
      val c =
        Class.forName(fqcn, false, classLoader).asInstanceOf[Class[_ <: T]]
      val t = implicitly[ClassTag[T]].runtimeClass
      if (t.isAssignableFrom(c)) c
      else throw new ClassCastException(s"$t is not assignable from $c")
    })

  private def createInstanceFor[T: ClassTag](
    clazz: Class[_],
    args: immutable.Seq[(Class[_], AnyRef)]
  ): Try[T] =
    Try {
      val types       = args.map(_._1).toArray
      val values      = args.map(_._2).toArray
      val constructor = clazz.getDeclaredConstructor(types: _*)
      constructor.setAccessible(true)
      val obj = constructor.newInstance(values: _*)
      val t   = implicitly[ClassTag[T]].runtimeClass
      if (t.isInstance(obj)) obj.asInstanceOf[T]
      else
        throw new ClassCastException(s"${clazz.getName} is not a subtype of $t")
    } recover {
      case i: InvocationTargetException if i.getTargetException ne null ⇒
        throw i.getTargetException
    }

  def createInstanceFor[T: ClassTag](
    fqcn: String,
    args: immutable.Seq[(Class[_], AnyRef)]
  ): Try[T] =
    getClassFor(fqcn) flatMap { c ⇒
      createInstanceFor(c, args)
    }
}
