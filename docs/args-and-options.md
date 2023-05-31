---
id: args-and-options
title: "Args and Options"
---

`Args[ArgsType]` and `Options[OptionsType]` represent CLI flags. They are descriptions of the process of constructing instances of `ArgsType` and `OptionsType` from a valid input.
They can be used to construct a `Command[(ArgsType, OptionsType)]` using one of the following methods:
```scala mdoc:silent
import zio.cli._

Command(name)
Command(name, options)
Command(name, args)
Command(name, options, args)
Command(name, helpDoc, options, args)
```

## Args
`Args[A]` is a description of the process of constructing an instance of `A` from a valid input of the CLI. It is not yet a specified argument for the CLI. In other words, an `Args[A]` defines a collection of valid arguments for a command and a way to construct a value `A` from them.

**ZIO CLI** offers a variety of methods to create basic `Args` that take a string from the user as input and transform it into a value of the corresponding type. 
```scala mdoc:silent
// Basic constructions
Args.bool(name)
Args.file(name)
Args.directory(name)
Args.text(name)
Args.decimal(name)
Args.integer(name)
Args.instant(name)
Args.localDate(name)
Args.localDateTime(name)
Args.localTime(name)
Args.monthDay(name)
Args.offsetDateTime(name)
Args.offsetTime(name)
Args.period(name)
Args.year(name)
Args.yearMonth(name)
Args.zonedDateTime(name)
Args.zoneId(name)
Args.zoneOffset(name)
```
We can also avoid specifying a name of the `Args`. In this case, the name will be the type:
```scala mdoc:silent
Args.bool(name) // Boolean Args with name
Args.bool       // Boolean Args named "boolean"
```

### Combining and transforming Args
It is possible to create more complex `Args` using the following methods of the trait `Args`:
```scala mdoc:silent
trait Args[A] {
  def ++[B](that: Args[B]): Args[(A, B)]              // Zip two args
  def +[A1 >: A]: Args[::[A1]]                        // Requires a non-empty list of Args of type A1
  def * : Args[List[A]]                               // Requires a list of Args of same type
  def atLeast(min: Int): Args[List[A]]                // Requires a list of Args with at least min elements
  def atMost(max: Int): Args[List[A]]                 // Requires a list of Args with at most max elements
  def between(min: Int, max: Int): Args[List[A]]      // Requires a list of Args with a bound on the number of elements
  def map[B](f: A => B): Args[B]                      // Applies a function f to the result of the Args
  def ??(that: String): Args[A]                       // Adds a string to the HelpDoc of the Args
}
```

### Example
We are going to construct an `Args` that asks for a list of 12 decimals (one for each month) and a year. Then it creates a new `Args` that stored the year and the mean value.
```scala mdoc:silent
import zio.cli._
val data: Args[BigDecimal] = Args.decimal
val args: Args[(BigDecimal, BigInt)] = data.between(12, 12) ++ Args.integer

case class YearAndMean(year: BigInt, mean: BigDecimal)

val mappedArgs: Args[YearAndMean] = args.map {
  case (months, year) = YearAndMean(year, months.sum()/12)
}
```

## Options
`Options[A]` is a description of the process of constructing an instance of `A` from a valid input of the CLI. It is not yet a specified option for the CLI. In other words, an `Options[A]` defines a collection of valid commands and a way to construct a value `A` from them.

The following methods construct an `Options` with a `name` that requires an input of the corresponding type from the user. Observe that for `Options` is necessary to specify the `name`.
```scala mdoc:silent
Options.boolean(name)
Options.file(name)
Options.directory(name)
Options.text(name)
Options.decimal(name)
Options.integer(name)
Options.instant(name)
Options.localDate(name)
Options.localDateTime(name)
Options.localTime(name)
Options.monthDay(name)
Options.offsetDateTime(name)
Options.offsetTime(name)
Options.period(name)
Options.year(name)
Options.yearMonth(name)
Options.zonedDateTime(name)
Options.zoneId(name)
Options.zoneOffset(name)
```
### Combining and transforming options
We can use the following methods to create more complex Options:
```scala mdoc:silent
trait Options[A] {
  def ++[A1 >: A, B](that: Options[B])
  def |[A1 >: A](that: Options[A1]): Options[A1]
  def orElse[A1 >: A](that: Options[A1]): Options[A1] // same as |
  def orElseEither[B](that: Options[B]): Options[Either[A, B]] 
  def map[B](f: A => B): Options[B] 
  def optional: Options[Option[A]] 

  // Creates options with different aliases
  def alias(name: String, names: String*): Options[A]

  // Creates options that have a default value
  def withDefault[A1 >: A](value: A1): Options[A1]
}
```

### Example
As an example, we are going to construct an instance of `Options[Order]` that defines two commands: ask for the path of a file and a string or a number to search in the file.
```scala mdoc:silent
import zio.cli._
// Construction of basic options.
val getFile: Options[JPath] = Options.file("file")
val getText: Options[String] = Options.text("textToSearch")
val getInt: Options[Int]   = Options.integer("intToSearch")

// First we construct a new Options[Either[String, Int]] that will allow to enter only one of the two possibilities in the CLI.
val getTextOrInt: Options[Either[String, Int]] = getText orElseEither getInt

// Then we are going to add some information that will appear if the user ask for help in the CLI App
val doc: String = "String or Int to search in the specified file"
val getTextOrIntWithDoc = getTextOrInt ?? doc

// Finally we combine getFile with getTextOrInt 
val options: Options[(JPath, Either[String, Int])] = getFile ++ getTextOrIntWithDoc

```
Note that we have not added any help to the `file` options or to `options` only to `getTextOrInt`.
The type of Options is cumbersome to work with. We might want to transform that type. This can be done using method `map`. This method transform the output from a valid input using `f`.

Defining a sealed trait to describe the output of the `Options` we can construct better `Options`.
```scala mdoc:silent
sealed trait Search
case class StringSearch(file: JPath, text: String) extends Search
case class IntSearch(file: JPath, int: Int) extends Search

// We need to specify a function `(JPath, Either[String, Int]) => Search`
val betterOptions: Options[Search] = options.map { (file, either) =>
  either match {
    case text: String => StringSearch(file, text)
    case int: Int     => IntSearch(file, int)
  }
}
```








 




