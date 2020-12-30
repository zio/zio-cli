package figlet

import figlet.FigFontRenderer.render
import figlet.FigFontRendererSpec.assertTextBlock
import zio.test._

object FigFontRendererJvmSpec extends DefaultRunnableSpec {
  def spec = suite("FigFontRendererSpec")(
    testM("ZIO-CLI!!! with standard.flf") {
      for {
        font_         <- FigFont.fromResource("standard.flf", getClass.getClassLoader)
        font: FigFont = font_ // TODO IJ cross-platform projects issue
        r             = render(font, "ZIO-\nCLI!!!")
      } yield {
        assertTextBlock(
          r,
          """
            | _____ ___  ___        |
            ||__  /|_ _|/ _ \       |
            |  / /  | || | | |_____ |
            | / /_  | || |_| |_____||
            |/____||___|\___/       |
            |  ____ _     ___  _ _ _ |
            | / ___| |   |_ _|| | | ||
            || |   | |    | | | | | ||
            || |___| |___ | | |_|_|_||
            | \____|_____|___|(_|_|_)|
            |                        |"""
        )
      }
    } @@ TestAspect.jvmOnly
  )
}
