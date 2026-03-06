package zio.cli

private[cli] trait ConfigFileResolverPlatformSpecific {

  lazy val live: ConfigFileResolver = ConfigFileResolver.none
}
