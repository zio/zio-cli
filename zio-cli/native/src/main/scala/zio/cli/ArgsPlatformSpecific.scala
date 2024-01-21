package zio.cli

import java.nio.file.{Path => JPath}

/**
 * A `Args` represents arguments that can be passed to a command-line application.
 */
private[cli] trait ArgsPlatformSpecific {

  import zio.cli.Args._

  /**
   * Creates a file argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   File argument
   */
  def file(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.File, exists))

  /**
   * Creates a file argument with 'file' as argument name
   *
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   File argument
   */
  def file(exists: Exists): Args[JPath] =
    Single(None, PrimType.Path(PathType.File, exists))

  /**
   * Creates a file argument with 'file' as argument name, and exists being 'Either'
   */
  val file: Args[JPath] = file(Exists.Either)

  /**
   * Creates a directory argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   Directory argument
   */
  def directory(name: String, exists: Exists = Exists.Either): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a directory argument with 'directory' as argument name
   *
   * @param exists
   *   Yes if path is expected to exists, No otherwise or Either is both are acceptable.
   * @return
   *   Directory argument
   */
  def directory(exists: Exists): Args[JPath] =
    Single(None, PrimType.Path(PathType.Directory, exists))

  /**
   * Creates a directory argument with 'directory' as argument name, and exists being 'Either'
   */
  val directory: Args[JPath] = directory(Exists.Either)

  /**
   * Creates a path argument with a custom argument name
   *
   * @param name
   *   Argument name
   * @return
   *   Path argument
   */
  def path(name: String): Args[JPath] =
    Single(Some(name), PrimType.Path(PathType.Either, Exists.Either))

  /**
   * Creates a path argument with 'path' as argument name
   */
  val path: Args[JPath] =
    Single(None, PrimType.Path(PathType.Either, Exists.Either))

}
