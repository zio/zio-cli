package zio.cli.figlet

import zio._

import java.io.IOException
import scala.io.{Codec, Source}
import zio.ZIO.attemptBlockingIO

/**
 * Adds the platform-specific constructors to the [[FigFont]] companion object. On the JVM they are `fromFile`,
 * `fromResource`, `fromURL` and `fromSource`; on Scala.js and Scala Native there are none.
 */
trait FigFontPlatformSpecific { self: FigFont.type =>

  /**
   * Reads the FIGfont file at the path `name`, decoded as ISO-8859-1, and parses it with `FigFont.fromLines`. Fails
   * with `Left` and the `java.io.IOException` when the file can't be read (a `java.io.FileNotFoundException` for a
   * missing file or a directory), or with `Right` and the parse error message.
   *
   * {{{
   * import zio._
   * import java.io.IOException
   *
   * val font: IO[Either[IOException, String], FigFont] = FigFont.fromFile("fonts/standard.flf")
   * }}}
   */
  final def fromFile(name: String): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromFile(name)(Codec.ISO8859)))

  /**
   * Reads the FIGfont resource `name` from `loader`, decoded with the JVM's default charset, and parses it with
   * `FigFont.fromLines`. Fails with `Right` and the parse error message when the resource is not a valid font. When
   * `loader` has no resource `name`, the effect dies with a `java.lang.NullPointerException` instead of failing.
   *
   * {{{
   * import zio._
   * import java.io.IOException
   *
   * val font: IO[Either[IOException, String], FigFont] =
   *   FigFont.fromResource("fonts/standard.flf", Thread.currentThread().getContextClassLoader)
   * }}}
   */
  final def fromResource(name: String, loader: ClassLoader): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromInputStream(loader.getResourceAsStream(name))))

  /**
   * Reads the FIGfont at `url`, decoded as ISO-8859-1, and parses it with `FigFont.fromLines`. Fails with `Left` and
   * the `java.io.IOException` when it can't be read (for example `java.net.MalformedURLException: unknown protocol:
   * foo` for `foo://x/y.flf`, or a `java.net.ConnectException`), or with `Right` and the parse error message.
   *
   * A string that is not a valid URI, such as `not a url`, makes the effect die with a `java.net.URISyntaxException` on
   * Scala 2.13 and 3; on Scala 2.12 it fails with `Left` and a `java.net.MalformedURLException`.
   *
   * {{{
   * import zio._
   * import java.io.IOException
   *
   * val font: IO[Either[IOException, String], FigFont] = FigFont.fromURL("https://example.com/fonts/standard.flf")
   * }}}
   */
  final def fromURL(url: String): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromURL(url)(Codec.ISO8859)))

  /**
   * Opens a `scala.io.Source` with the effect `source`, reads all its lines, closes it, and parses the lines with
   * `FigFont.fromLines`. Fails with `Left` and the error of `source` or the `java.io.IOException` of reading, or with
   * `Right` and the parse error message. The type parameter `A` is not used.
   *
   * {{{
   * import zio._
   * import java.io.IOException
   * import scala.io.Source
   *
   * val font: IO[Either[IOException, String], FigFont] =
   *   FigFont.fromSource(ZIO.attemptBlockingIO(Source.fromFile("fonts/standard.flf")))
   * }}}
   */
  final def fromSource[R <: Any, A, E >: IOException](
    source: => ZIO[R, E, Source]
  ): ZIO[R, Either[E, String], FigFont] =
    for {
      lines <- ZIO
                 .acquireReleaseWith(source)(source => ZIO.succeed(source.close))(s =>
                   attemptBlockingIO((Chunk.fromIterator((s.getLines()))))
                 )
                 .mapError(Left(_))
      font <- ZIO
                .fromEither(self.fromLines(lines))
                .mapError(Right(_))
    } yield font
}
