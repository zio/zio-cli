package zio.cli.figlet

import FigFontRenderer.render
import FigFontRendererSpec.assertTextBlock
import zio.test._
import zio.test.ZIOSpecDefault

object FigFontRendererJvmSpec extends ZIOSpecDefault {
  def spec = suite("FigFontRendererJvmSpec")(
    test("ZIO-CLI!!! with standard.flf") {
      for {
        font <- FigFont.fromResource("standard.flf", getClass.getClassLoader)
        r     = render(font, "ZIO-\nCLI!!!")
      } yield {
        assertTextBlock(
          r,
          """
            | ________ ___        |
            ||__  /_ _/ _ \       |
            |  / / | | | | |_____ |
            | / /_ | | |_| |_____||
            |/____|___\___/       |
            |  ____ _     ___ _ _ _ |
            | / ___| |   |_ _| | | ||
            || |   | |    | || | | ||
            || |___| |___ | ||_|_|_||
            | \____|_____|___(_|_|_)|
            |                       |"""
        )
      }
    } @@ TestAspect.jvmOnly
  )
}
