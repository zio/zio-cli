package zio.cli

trait FileArgsPlatformSpecific {
  val default: FileArgs = FileArgs.Live
}
