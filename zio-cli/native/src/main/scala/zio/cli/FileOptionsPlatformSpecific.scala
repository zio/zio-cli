package zio.cli

private[cli] trait FileOptionsPlatformSpecific {
  val default: FileOptions = FileOptions.Live
}
