package zio.cli.completion
import zio.cli.ShellType
import zio.cli.PathPlatformSpecific

object CompletionScript extends PathPlatformSpecific {
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
