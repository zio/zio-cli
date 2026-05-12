package zio.cli

private[cli] trait FileOptionsPlatformSpecific {
  // Scala.js has no filesystem in the browser and only a virtual one in Node, so we default to a no-op
  // and let users opt in to a custom implementation via `runWithFileArgs`.
  val default: FileOptions = FileOptions.Noop
}
