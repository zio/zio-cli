package zio.cli

import scala.annotation.tailrec

object Completions {
  def getSubcommands(input: List[String], command: Command[_]): List[Command[_]] = {
    val query = input.headOption.getOrElse("")
    val result = command match {
      case single: Command.Single[_, _] =>
        // TODO: Get clever about the Options and Args matching, if query is a perfect fit...
        // Watch the Zymposium on this to remember.
        if (single.names.exists(_.startsWith(query))) List(single)
        else List.empty
      case Command.OrElse(left, right) => getSubcommands(input, left) ++ getSubcommands(input, right)
      case Command.Map(command, _)     => getSubcommands(input, command)
      case Command.Subcommands(parent, child) =>
        if (parent.names.contains(query)) getSubcommands(input.drop(1), child)
        else if (parent.names.exists(_.startsWith(query))) List(parent)
        else List.empty
    }

    result
  }

  val pet =
    Command("fet", Options.none, Args.none)
  val treat =
    Command("treat", Options.none, Args.none)
  val food =
    Command("food", Options.none, Args.none)
  val feed =
    Command("feed", Options.none, Args.none).subcommands(treat | food)
  val dispose =
    Command("dispose", Options.none, Args.none)
  val tamo =
    Command("tamo", Options.none, Args.none).subcommands(pet | feed | dispose)

  def main(args: Array[String]): Unit =
    println(generateCompletions("feed t", tamo))

  @tailrec
  private def prefix(command: Command[_]): List[String] =
    command match {
      case Command.Single(name, _, _, _)  => List(name)
      case Command.Map(command, _)        => prefix(command)
      case Command.OrElse(_, _)           => Nil
      case Command.Subcommands(parent, _) => prefix(parent)
    }

  def generateCompletions(input: String, command: Command[_]): List[String] = {
    val inputList = prefix(command) ++ input.split(" ").toList
    getSubcommands(inputList, command).flatMap(_.names.toList)
  }
}
