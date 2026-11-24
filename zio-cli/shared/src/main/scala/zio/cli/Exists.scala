package zio.cli

/**
 * Describes whether the command-line application wants a file/directory to exist, not to exist, or doesn't care. `PrimType.Path` checks it before the kind of the path.
 */
sealed trait Exists
/** The three existence rules for a path. */
object Exists {

  /**
   * The path must exist; a missing path fails with `Path 'out' must exist.` for the input `out`.
   */
  case object Yes extends Exists

  /**
   * The path must not exist; an existing path fails with `Path 'out' must not exist.` for the input `out`. The kind of the path is not checked.
   */
  case object No extends Exists

  /**
   * The path may exist or not. A missing path is accepted without a kind check.
   */
  case object Either extends Exists
}
