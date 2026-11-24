package zio.cli

/**
 * Describes whether the command-line application wants a path to be a file, a directory, or either. `PrimType.Path` checks the kind only for a path that exists, and not when the path must not exist (`Exists.No`).
 */
sealed trait PathType
/** The three kinds of path that a path parameter can expect. */
object PathType {

  /** Accepts a file or a directory: no kind check. */
  case object Either extends PathType

  /**
   * Expects a regular file; an existing directory fails with `Expected path 'out' to be a regular file.` for the input `out`.
   */
  case object File extends PathType

  /**
   * Expects a directory; an existing file fails with `Expected path 'out' to be a directory.` for the input `out`.
   */
  case object Directory extends PathType
}
