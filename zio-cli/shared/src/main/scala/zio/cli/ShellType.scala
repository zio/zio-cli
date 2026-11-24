package zio.cli

/**
 * The shell that a completion script or a list of completions is for. The built-in `--shell-type` option reads it.
 *
 * @see [[https://zio.dev/zio-cli/bash-and-zsh-completion/ Bash Completion]]
 */
sealed trait ShellType
/** The supported shells and the built-in `--shell-type` option. */
object ShellType {

  /** Bash, selected by `--shell-type bash` or `--shell-type sh`. */
  case object Bash extends ShellType

  /**
   * Z shell, selected by `--shell-type zsh`. There is no completion script for it: `CompletionScript` throws `scala.NotImplementedError` for `ZShell`, so `--shell-completion-script` with `--shell-type zsh` prints no script.
   */
  case object ZShell extends ShellType

  /**
   * The built-in `--shell-type` option: `sh` and `bash` give `Bash`, and `zsh` gives `ZShell`. The words must match exactly, so `BASH` fails with `Expected one of the following cases: sh, bash, zsh`.
   */
  val option: Options[ShellType] =
    Options.enumeration("shell-type")(
      "sh"   -> Bash,
      "bash" -> Bash,
      "zsh"  -> ZShell
    )
}
