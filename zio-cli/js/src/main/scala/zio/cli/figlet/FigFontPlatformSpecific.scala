package zio.cli.figlet

/**
 * Adds the platform-specific constructors to the [[FigFont]] companion object. On the JVM they are `fromFile`,
 * `fromResource`, `fromURL` and `fromSource`; on Scala.js and Scala Native there are none.
 */
trait FigFontPlatformSpecific { self: FigFont.type => }
