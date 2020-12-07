package figlet

import zio._
import zio.blocking.{ effectBlockingIO, Blocking }

import java.io.IOException
import scala.io.Source

trait FigFontPlatformSpecific { self: FigFont.type =>
  final def fromResource(name: String, loader: ClassLoader): ZIO[Blocking, Either[IOException, String], FigFont] =
    for {
      lines <- ZManaged
                .fromAutoCloseable(effectBlockingIO(loader.getResourceAsStream(name)))
                .use { s =>
                  effectBlockingIO(Source.fromInputStream(s).getLines().to(Seq))
                }
                .mapError(Left(_))
      figFont <- ZIO.fromEither(self.fromLines(lines)).mapError(Right(_))
    } yield figFont
}
