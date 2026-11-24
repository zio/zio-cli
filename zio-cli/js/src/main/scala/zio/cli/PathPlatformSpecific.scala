package zio.cli

/**
 * Gives a type its platform's path type as `JPath`.
 */
private[cli] trait PathPlatformSpecific {

  /**
   * The path type of the platform: `java.nio.file.Path` on the JVM and Scala Native, `String` on Scala.js.
   */
  type JPath = String

}

/**
 * Holds the path type of the platform as `JPath`.
 */
private[cli] object PathPlatformSpecific {

  /**
   * The path type of the platform: `java.nio.file.Path` on the JVM and Scala Native, `String` on Scala.js.
   */
  type JPath = String

}
