package zio.cli.figlet

import zio._
import zio.blocking.{effectBlockingIO, Blocking}

import java.io.IOException
import scala.io.{Codec, Source}

trait FigFontPlatformSpecific { self: FigFont.type =>
  final def fromFile(name: String): ZIO[Blocking, Either[IOException, String], FigFont] =
    fromSource(effectBlockingIO(Source.fromFile(name)(Codec.ISO8859)))

  final def fromResource(name: String, loader: ClassLoader): ZIO[Blocking, Either[IOException, String], FigFont] =
    fromSource(effectBlockingIO(Source.fromInputStream(loader.getResourceAsStream(name))))

  final def fromURL(url: String): ZIO[Blocking, Either[IOException, String], FigFont] =
    fromSource(effectBlockingIO(Source.fromURL(url)(Codec.ISO8859)))

  final def fromSource[R <: Blocking, A, E >: IOException](
    source: => ZIO[R, E, Source]
  ): ZIO[R, Either[E, String], FigFont] =
    for {
      lines <- ZManaged
                 .fromAutoCloseable(source)
                 .use(s => effectBlockingIO(s.getLines().toSeq))
                 .mapError(Left(_))
      font <- ZIO
                .fromEither(self.fromLines(lines))
                .mapError(Right(_))
    } yield font
}
