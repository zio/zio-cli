package zio.cli

object Completion {
  def complete(shellType: ShellType, words: List[String], index: Int): List[String] = {
    // TODO: The dummy completions below are just a proof of concept. They
    // should be replated with legitimate completions.
    val dummyCompletions = (
      words.zipWithIndex.map{case (word, i) => s"$word-$i"} ++
      List(s"cursor-index-$index") ++
      List(s"shell-is-$shellType") ++
      "Mini-Me You Complete Me".split(" ").toList
    )
    dummyCompletions
  }
}
