package figlet

import zio._
import zio.blocking._
import zio.console.putStrLn
import zio.test._
import zio.test.Assertion._

import scala.io.Source
import java.nio.file._
import scala.util.Using

object FigFontRenderReportSpec extends DefaultRunnableSpec {
  private val fontDbUrl = "http://www.figlet.org/fontdb.cgi"
  private val fontUrl   = "http://www.figlet.org/fonts/"

  override def spec = suite("FigFontRenderReportSpec")(
    testM("figlet.org Fonts Render Report") {
      for {
        fontDbHtml <- ZManaged
                       .fromAutoCloseable(effectBlockingIO(Source.fromURL(fontDbUrl)))
                       .use(s => effectBlockingIO(s.getLines().mkString))
        names = "(?<=\\?font=)[\\w-]+\\.flf".r.findAllIn(fontDbHtml).toSeq
        items <- ZIO.foreachPar(names) { name =>
                  val url = s"$fontUrl$name"
                  FigFont
                    .fromURL(url)
                    .fold({
                      case Left(e)  => Left(e.toString)
                      case Right(s) => Left(s)
                    }, f => renderOrError(f))
                    .map(r => (url, r))
                }
        title = s"$fontUrl Render Report"
        path  <- renderReport(title, items)
        _     <- putStrLn(s"$title: $path")
      } yield assert(items.collect { case (url, Left(s)) => (url, s) })(isEmpty)
    } @@ TestAspect.ignore
//    }
  )

  private def renderOrError(font: FigFont) =
    try {
      val (abc, rest)    = font.chars.keys.partition(_.isLetter)
      val (lower, upper) = abc.partition(_.isLower)
      val sample         = Seq(lower, upper, rest).map(_.mkString).mkString("\n")
      Right(FigFont.render(font, sample))
    } catch {
      case t: Throwable => Left(t.toString)
    }

  private def renderReport(title: String, items: Seq[(String, Either[String, Chunk[String]])]) = effectBlocking {
    val path = Files.createTempFile("figlet-report", ".md")
    Using(Files.newBufferedWriter(path)) { w =>
      w.write(s"# $title\n")
      items.foreach {
        case (url, Left(s)) =>
          w.write(s"\n* __[FAILED]__ <$url>\n```\n")
          w.write(s)
          w.write("```\n")
        case (url, Right(lines)) =>
          w.write(s"\n* <$url>\n```\n")
          lines.foreach { l =>
            w.write(l)
            w.write('\n')
          }
          w.write("```\n")
      }
    }
    path.toUri.toString
  }
}
