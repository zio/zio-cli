---
id: args
title: "Args"
---
The `Args` data type models command-line arguments. Arguments are name-less, position-based parameters passed to a command-line program that modify its behavior. For example, the Git CLI has a command named `git clone` that creates a copy of an existing repository. It has an argument called "repository" that represents the existing repository's path. To clone the **ZIO CLI** repository, it can be called this way:
```
git clone https://github.com/zio/zio-cli.git
```
where `https://github.com/zio/zio-cli.git` is the path of the repository.

In **ZIO CLI**, Arguments are represented by instances of class `Args[_]`. `Args[A]` is a description of the process of constructing an instance of `A` from a valid input of the CLI. It is not yet a specified argument for the CLI. In other words, an `Args[A]` defines a collection of valid arguments for a command and a way to construct a value `A` from them.


## Construction of basic Args
**ZIO CLI** offers a variety of methods to create basic `Args` that takes a string from the user as input and transform it into a value of the corresponding type. The benefit of using these methods is that it is automatically checked if they describe a value of the given type and, in that case, are transformed into it. In case of an invalid input, the parser returns a `ValidationError` failure. Note that each type has its own validation function.

An `Args` instance of a basic type carries a name attribute. This can be specified when being created, but it can also be skipped. In this last case, the name will be that of the type. An example with Boolean type:
```scala mdoc:silent
import zio.cli._

val name = "name"

Args.bool(name) // Boolean Args with name
Args.bool       // Boolean Args named "boolean"
```



### Boolean Args
Produces an `Args[Boolean]`. It accepts `true` or `false`, and the case-insensitive aliases `1`/`0`, `y`/`n`, `yes`/`no` and `on`/`off`. Other text is not valid.
```scala mdoc:silent
Args.bool(name)
```
### Path Args
They are used to produce an `Args[Path]`.
- Produces `Args[java.nio.file.Path]`.
```scala mdoc:silent
Args.file(name) // Used for a file
```
- Produces `Args[java.nio.file.Path]`.
```scala mdoc:silent
Args.directory(name) // Used for a directory
```
By default, the `CliApp` accepts paths pointing to files or directories that might or not exist. If you need to work with one of them exclusively you can use the trait `Exists`:
```scala mdoc:silent
// Path can point to both existing or non-existing files or directories
Args.file(name, exists = Exists.Either)
Args.directory(name, exists = Exists.Either)

// Path must point to existing files or directories
Args.file(name, exists = Exists.Yes)
Args.directory(name, exists = Exists.Yes)

// Path must point to non-existing files or directories
Args.file(name, exists = Exists.No)
Args.directory(name, exists = Exists.No)
```
### Text Args
Produces an `Args[String]`.
```scala mdoc:silent
Args.text(name)
```
### Numeric Args
Numeric `Args` produce `BigInt` and `BigDecimal`, so they can represent numbers of arbitrary size and precision. Note that the CLI checks that the input really represents a number.
- Produces `Args[BigDecimal]`.
```scala mdoc:silent
Args.decimal(name)
```

- Produces `Args[BigInt]`.
```scala mdoc:silent
Args.integer(name)
```

### Date/Time Args
The following methods produce `Args` whose type parameter is a Date/Time type of the `java.time` library.
- Produces `Args[java.time.Duration]`. The input must be a time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.
```scala mdoc:silent
Args.duration(name)
```
- Produces `Args[java.time.Instant]`. The input must be an instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
```scala mdoc:silent
Args.instant(name)
```
- Produces `Args[java.time.LocalDate]`. The input must be a date in ISO_LOCAL_DATE format, such as 2007-12-03.
```scala mdoc:silent
Args.localDate(name)
```
- Produces `Args[java.time.LocalDateTime]`. The input must be a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
```scala mdoc:silent
Args.localDateTime(name)
```
- Produces `Args[java.time.LocalTime]`. The input must be a time without a time-zone in the ISO-8601 format, such as 10:15:30.
```scala mdoc:silent
Args.localTime(name)
```
- Produces `Args[java.time.MonthDay]`. The input must be a month-day in the ISO-8601 format `--MM-dd`, such as `--12-03`.
```scala mdoc:silent
Args.monthDay(name)
```
- Produces `Args[java.time.OffsetDateTime]`. The input must be a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.
```scala mdoc:silent
Args.offsetDateTime(name)
```
- Produces `Args[java.time.OffsetTime]`. The input must be a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.
```scala mdoc:silent
Args.offsetTime(name)
```
- Produces `Args[java.time.Period]`. The input must be a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
```scala mdoc:silent
Args.period(name)
```
- Produces `Args[java.time.Year]`. The input must be a plain integer year, such as 2007.
```scala mdoc:silent
Args.year(name)
```
- Produces `Args[java.time.YearMonth]`. The input must be a year-month in the ISO-8601 format, such as 2007-12.
```scala mdoc:silent
Args.yearMonth(name)
```
- Produces `Args[java.time.ZonedDateTime]`. The input must be an ISO-8601 offset date-time followed by the zone ID in square brackets, such as `2007-12-03T10:15:30+01:00[Europe/Paris]`.
```scala mdoc:silent
Args.zonedDateTime(name)
```
- Produces `Args[java.time.ZoneId]`. The input must be a time-zone ID, such as Europe/Paris.
```scala mdoc:silent
Args.zoneId(name)
```
- Produces `Args[java.time.ZoneOffset]`. The input must be a time-zone offset from Greenwich/UTC, such as +02:00.
```scala mdoc:silent
Args.zoneOffset(name)
```

### Enumeration Args
Produces an `Args[A]` that accepts one of a fixed set of words. Each case is a pair of the word the user types and the value it stands for. The value is the second element of the pair. `Args.enumeration(name)(cases*)` gives the argument a name, and `Args.enumeration(cases*)` uses `choice` as the name.
```scala mdoc:silent
Args.enumeration("level")(("debug", 0), ("info", 1), ("warn", 2)) // Args[Int], named "level"
Args.enumeration(("debug", 0), ("info", 1), ("warn", 2))         // Args[Int], named "choice"
```

### Other constructors
- `Args.none` produces an `Args[Unit]` that consumes no input. Use it for a command that takes no arguments:
```scala mdoc:silent
Command("status", Options.boolean("short"), Args.none)
```
- `Args.path(name)` and `Args.path` produce an `Args[java.nio.file.Path]`. The path may be a file or a directory, and it may or may not exist. The unnamed form uses `path` as the name. This constructor is available on the JVM and Native only (see [Platform support](#platform-support)).
```scala mdoc:silent
Args.path("target")
```

### Platform support
- On the JVM and Native, `Args.file`, `Args.directory` and `Args.path` are available.
- On Scala.js, none of them are available. Code that uses them does not compile for Scala.js.
- The matching options work differently. `Options.file` is available on all three platforms. `Options.directory` is not available on Scala.js. See [Options](options.md).

### Primitive types
Each basic constructor, such as `Args.text` or `Args.file`, creates a `PrimType`. A `PrimType` knows how to read a string from the command line, check it, and describe the accepted values in the help output. `PrimType` is a sealed trait, so the library defines the complete set of types and you cannot add new ones from outside.

Path arguments use three small types:
- `PathType` says what kind of path is expected: `PathType.File`, `PathType.Directory`, or `PathType.Either` when both are acceptable.
- `Exists` says whether the path must exist: `Exists.Yes`, `Exists.No`, or `Exists.Either` when it does not matter.
- `PrimType.Path(pathType, shouldExist)` combines the two. For example, `Args.file(name, exists = Exists.Yes)` is built from `PrimType.Path(PathType.File, Exists.Yes)`.
```scala mdoc:silent
PrimType.Path(PathType.File, Exists.Yes)
```

## Combining and transforming Args
When constructing a command, you can specify only one argument. Thus, to create more complex `Args` it is necessary to use the following methods of the trait `Args`:
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

### Adding Args
Operator `++` can be used to zip two arguments. It can be use to chain two arguments in a tuple. For example, `git clone` command has arguments `<repository>` and `<directory>`. The argument of this command can be created in the following manner:
```scala mdoc:silent
val cloneArgs = Args.text("repository") ++ Args.text("directory")
```

The output of the CLI help will be:
```
COMMANDS

  clone <repository> <directory>
```

### Repeating Args
If we need an argument a repeated number of times, we can use the following operators:
- Method `*`

It creates a new `Args` that accepts a list of arguments of the same type. There are no restrictions on length. Take into account that there is no limit, so if there are arguments or options after using this methods, the CLI app will not read them unless they are not valid!
```scala mdoc:silent
// Accepts a list, possibly empty, of texts.
Args.text.*
```
- Method `+`

It works as method `*` but the list of arguments cannot be empty. Do not mistake with method `++`!

- Method `between`

It creates an argument accepting a list of arguments of the type before whose length must be between `min` and `max` parameters.
```scala mdoc:silent
Args.text.between(2,5) // Creates an arguments accepting a list of String of length between 2 and 5.
```
- Method `atLeast`

It creates an argument accepting a list of arguments of the type before whose length must be at least the `min` parameter.
```scala mdoc:silent
Args.text.atLeast(2) // Creates an arguments accepting a list of String of length at least 2.
```

- Method `atMost`

It creates an argument accepting a list of arguments of the type before whose length must be at most the `max` parameter.
```scala mdoc:silent
Args.text.atMost(5) // Creates an arguments accepting a list of String of length at most 5.
```

### Transforming Args
Method `map` allows to transform the type parameter of `Args[A]`. It takes a function `f: A => B` as parameter that is applied when processing a user's input in a CLI app and returns `Args[B]`. This makes it easier to implement the business logic of a CLI app. For example, we will construct an `Args` that asks for a list of 12 decimals (one for each month) and a year. Then, it will create a new `Args` that store the year and the mean value in a custom type.
```scala mdoc:silent:reset
import zio.cli._

val data: Args[BigDecimal] = Args.decimal
val args: Args[(List[BigDecimal], BigInt)] = data.between(12, 12) ++ Args.integer

case class YearAndMean(year: BigInt, mean: BigDecimal)

val mappedArgs: Args[YearAndMean] = args.map {
  case (months, year) => YearAndMean(year, months.sum/12)
}
```

### Adding help
Method `??` allows adding information about an argument. The string is added after the current `HelpDoc` of the `Args`. We are going to create the `<repository>` argument of `git clone` to observe the effect of using `??`.

```scala mdoc:silent
val repository = Args.text("repository")
  
/* HelpDoc of repository:
 *
 * <repository>
 *   A user-defined piece of text.
 * 
 */

```
Now we add a description of the argument:

```scala mdoc:silent
val repositoryWithHelp = repository ?? "Path of the repository to be cloned."

/* HelpDoc of repositoryWithHelp:
 *
 * <repository>
 *   A user-defined piece of text.
 * 
 *   Path of the repository to be cloned.
 */
```

