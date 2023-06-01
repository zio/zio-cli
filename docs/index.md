---
id: index
title: "Introduction to ZIO CLI"
sidebar_label: "ZIO CLI"
---

Rapidly build powerful command-line applications powered by ZIO

@PROJECT_BADGES@

## Installation

To use **ZIO CLI**, we need to add the following to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-cli" % "@VERSION@"
```
## Getting Started
**ZIO CLI** allows to easily construct a CLI application. This is done defining `cliApp` value from `ZIOCliDefault` using `CliApp.make` and specifying a `Command` as parameter. A `Command[Model]` is a description of the commands of a CLI application that allows to specify which commands are valid and how to transform the input into an instance of `Model`. Then it is possible to implement the logic of the CLI application in terms of `Model`. 

```scala mdoc
import zio.cli._
import zio.cli.HelpDoc.Span.text

// object of your app must extend ZIOCliDefault
object Sample extends ZIOCliDefault {

  /**
   * First we define the commands of the Cli. To do that we need:
   *    - Create command options
   *    - Create command arguments
   *    - Create help (HelpDoc) 
   */
  val options: Options[String] = Options.text("textOption")
  val arguments: Args[BigInt] = Args.integer("intArguments")
  val help: HelpDoc = HelpDoc.p("cli help")
  
  val command: Command[(String, BigInt)] = Command("command", options, arguments).withHelp(help)
  
  // Define val cliApp using CliApp.make
  val cliApp = CliApp.make(
    name = "Sample",
    version = "1.1.0",
    summary = text("Sample cli"),
    command = command
  ) {
    // Implement logic of CliApp
    case (string, bigInteger) => ???
  }
}
```

If there is a `CliApp`, you can run a command using its method `run` and passing parameters in a `List[String]`.

## References

- [10 Minute Command-Line Apps With ZIO CLI](https://www.youtube.com/watch?v=UeR8YUN4Tws) by Aiswarya Prakasan
- [Hacking on ZIO-CLI](https://www.youtube.com/watch?v=HxPCXfnbg3U) by Adam Fraser and Kit Langton
- [Behold! The Happy Path To Captivate Your Users With Stunning CLI Apps!](https://www.youtube.com/watch?v=0c3zbUq4lQo) by Jorge Vasquez
