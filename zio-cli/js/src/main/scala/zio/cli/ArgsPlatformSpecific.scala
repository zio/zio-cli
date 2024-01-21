package zio.cli

import zio._
import zio.cli.HelpDoc.{Span, p}

/**
 * A `Args` represents arguments that can be passed to a command-line application.
 */
private[cli] trait ArgsPlatformSpecific {}
