package zio.cli.figlet

import zio._

import java.io.IOException
import scala.io.{Codec, Source}
import zio.ZIO.attemptBlockingIO

trait FigFontPlatformSpecific { self: FigFont.type =>
  final def fromFile(name: String): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromFile(name)(Codec.ISO8859)))

  final def fromResource(name: String, loader: ClassLoader): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromInputStream(loader.getResourceAsStream(name))))

  final def fromURL(url: String): ZIO[Any, Either[IOException, String], FigFont] =
    fromSource(attemptBlockingIO(Source.fromURL(url)(Codec.ISO8859)))

  final def fromSource[R <: Any, A, E >: IOException](
    source: => ZIO[R, E, Source]
  ): ZIO[R, Either[E, String], FigFont] =
    for {
      lines <- ZManaged
                 .acquireReleaseWith(source)(source => UIO(source.close()))
                 .use(s => attemptBlockingIO(Chunk.fromIterator(s.getLines())))
                 .mapError(Left(_))
      font <- ZIO
                .fromEither(self.fromLines(lines))
                .mapError(Right(_))
    } yield font
}
