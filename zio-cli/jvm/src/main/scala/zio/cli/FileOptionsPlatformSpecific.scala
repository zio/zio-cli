package zio.cli

private[cli] trait FileOptionsPlatformSpecific {
  // JVM has full filesystem access, so the live implementation walks dotfiles for real.
  val default: FileOptions = FileOptions.Live
}
