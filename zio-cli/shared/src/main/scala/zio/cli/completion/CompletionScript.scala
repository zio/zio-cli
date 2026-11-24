package zio.cli.completion
import zio.cli.ShellType
import zio.cli.PathPlatformSpecific

/**
 * Generates the shell script that sets up tab completion for a CLI program. `CliApp#run` prints it for the built-in
 * options `--shell-completion-script <path> --shell-type bash`.
 */
object CompletionScript extends PathPlatformSpecific {

  /**
   * Returns the Bash completion script for the programs named in `programNames`. The script defines a function named
   * `_` followed by the first name of `programNames` in the set's iteration order; when the user presses tab, it
   * exports the words of the command line as the environment variables `COMP_WORD_0`, `COMP_WORD_1`, and so on, runs
   * `pathToExecutable --shell-type bash --shell-completion-index <index of the word under the cursor>`, and offers each
   * line that the program prints as a completion. The script ends with one `complete -F` line for each name in
   * `programNames`.
   *
   * Throws `scala.NotImplementedError` for `ShellType.ZShell`, and `java.util.NoSuchElementException` when
   * `programNames` is empty.
   *
   * {{{
   * import zio.cli.ShellType
   *
   * // On the JVM, JPath is java.nio.file.Path.
   * val script: String =
   *   CompletionScript(java.nio.file.Paths.get("/usr/local/bin/git-cli"), Set("git"), ShellType.Bash)
   * // The last line of `script` is: complete -F _git git
   * }}}
   */
  def apply(pathToExecutable: JPath, programNames: Set[String], shellType: ShellType): String = shellType match {
    case ShellType.Bash   => bash(pathToExecutable, programNames)
    case ShellType.ZShell => ???
  }
  private[this] def bash(pathToExecutable: JPath, programNames: Set[String]): String =
    s"""|#/usr/bin/env bash
        |_${programNames.head}()
        |{
        |  local CMDLINE
        |  local IFS=$$'\\n'
        |  CMDLINE=(--shell-type bash --shell-completion-index $$COMP_CWORD)
        |  
        |  INDEX=0
        |  for arg in $${COMP_WORDS[@]}; do
        |    export COMP_WORD_$$INDEX=$${arg}
        |    (( INDEX++ ))
        |  done
        |
        |  COMPREPLY=( $$($pathToExecutable \"$${CMDLINE[@]}\") )
        |
        |  # Unset the environment variables.
        |  unset $$(compgen -v | grep "^COMP_WORD_")
        |}
        |
        |""".stripMargin + programNames
      .map(programName => s"complete -F _${programNames.head} $programName")
      .mkString(System.lineSeparator)
}
