package zio.cli

private[cli] trait FileOptionsPlatformSpecific {
  // Scala Native exposes a usable subset of java.nio.file, so the live implementation works there too.
  val default: FileOptions = FileOptions.Live
}
