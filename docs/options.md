---
id: options
title: "Options"
---
The `Options` data type models command-line options. Contrary to arguments, options are named, position-independent parameters passed to a command-line program that modify its behavior. Note that the user must specify the name of the option just before its content. As an example, the Git CLI has a command named `git checkout` that allows changing between different development branches. It has different options that modify the functionality of the command like option `quiet`. This option allows suppressing feedback messages. It can be specified in the following manners:
```
git checkout --quiet
git checkout -q
```
Both `--quiet` and `-q` refer to the same option. `-q` is the alias of `quiet`, a shorter form of the option. Note that a one-character alias is preceded by `-`, while a longer alias is preceded by `--`.

In **ZIO CLI**, Options are represented by instances of class `Options[_]`. `Options[A]` is a description of the process of constructing an instance of `A` from a valid input of the CLI. It is not yet a specified option for the CLI. In other words, an instance of `Options[A]` defines a collection of valid commands and a way to construct a value `A` from them.

## Construction of basic Options
**ZIO CLI** offers a variety of methods to create basic `Options` that take a string from the user as input and transform it into a value of the corresponding type. The benefit of using these methods is that it is automatically checked if they describe a value of the given type and, in that case, are transformed into it. In case of an invalid input, the parser returns a `ValidationError` failure.

The following methods construct an `Options` with a `name` that requires an input of the corresponding type from the user. Observe that for `Options` it is mandatory to specify the `name`.
### Boolean Options
Produces an `Options[Boolean]`. It accepts `true` or `false`, and the case-insensitive aliases `1`/`0`, `y`/`n`, `yes`/`no` and `on`/`off`. Other text is not valid. A flag such as `--quiet` with no value means `true`.
```scala mdoc:silent
import zio.cli._

val name = "name"

Options.boolean(name)
```
### Boolean Options with a negation
`Options.boolean(name, ifPresent, negationName, negationNames*)` adds a second name that sets the opposite value. With `ifPresent` set to `true`, `--color` gives `true` and `--no-color` gives `false`. If neither name is given, the value is `false`, which is the opposite of `ifPresent`. Giving both names in one command is a validation error.
```scala mdoc:silent
Options.boolean("color", true, "no-color")
```
The overload `Options.boolean(name, alias, ifPresent, negationName, negationNames*)` also gives the main name a single alias, such as `-v` for `--verbose`.
```scala mdoc:silent
Options.boolean("verbose", "v", true, "quiet", "q")
```

### Enumeration Options
If we desire to allow the user to choose between different options, we can use the `enumeration` method. It must specify a name and tuples `(String, A)` to create an instance of `Options[A]`. A valid input from the user is a key of a tuple and it will produce the corresponding value. We can use this method if there are different ways to execute a command. As an example, the command `git commit` allows choosing between options `--all`, `--interactive` and `--patch` that control how the user specifies the changes to be committed. One way to implement this would use `enumeration`:
```scala mdoc:silent
sealed trait Mode
case class All() extends Mode
case class Interactive() extends Mode
case class Patch() extends Mode

Options.enumeration[Mode]("mode")(("all", All()), ("interactive", Interactive()), ("patch", Patch()))
```
The output produced by the HelpDoc is
```
OPTIONS

  --mode all | interactive | patch
    One of the following cases: all, interactive, patch.
```

### Path Options
They are used to produce an `Options[Path]`.
- Produces `Options[java.nio.file.Path]`.
```scala mdoc:silent
Options.file(name) // Used for a file
```
- Produces `Options[java.nio.file.Path]`.
```scala mdoc:silent
Options.directory(name) // Used for a directory
```
By default, the `CliApp` accepts paths pointing to files or directories that might or not exist. If you need to work with one of them exclusively you can use the type `Exists`:
```scala mdoc:silent
// Path can point to both existing or non-existing files or directories
Options.file(name, exists = Exists.Either)
Options.directory(name, exists = Exists.Either)

// Path must point to existing files or directories
Options.file(name, exists = Exists.Yes)
Options.directory(name, exists = Exists.Yes)

// Path must point to non-existing files or directories
Options.file(name, exists = Exists.No)
Options.directory(name, exists = Exists.No)
```
### Text Options
Produces an `Options[String]`.
```scala mdoc:silent
Options.text(name)
```
### Numeric Options
Numeric `Options` produce `BigInt` and `BigDecimal`, so they can represent numbers of arbitrary size and precision. Note that the CLI checks that the input really represents a number.
- Produces `Options[BigDecimal]`.
```scala mdoc:silent
Options.decimal(name)
```

- Produces `Options[BigInt]`.
```scala mdoc:silent
Options.integer(name)
```

### Date/Time Options
The following methods produce `Options` whose type parameter is a Date/Time type of the `java.time` library.
- Produces `Options[java.time.Duration]`. The input must be a time-based amount of time in the ISO-8601 format, such as 'P1DT2H3M'.
```scala mdoc:silent
Options.duration(name)
```
- Produces `Options[java.time.Instant]`. The input must be an instant in time in UTC format, such as 2007-12-03T10:15:30.00Z.
```scala mdoc:silent
Options.instant(name)
```
- Produces `Options[java.time.LocalDate]`. The input must be a date in ISO_LOCAL_DATE format, such as 2007-12-03.
```scala mdoc:silent
Options.localDate(name)
```
- Produces `Options[java.time.LocalDateTime]`. The input must be a date-time without a time-zone in the ISO-8601 format, such as 2007-12-03T10:15:30.
```scala mdoc:silent
Options.localDateTime(name)
```
- Produces `Options[java.time.LocalTime]`. The input must be a time without a time-zone in the ISO-8601 format, such as 10:15:30.
```scala mdoc:silent
Options.localTime(name)
```
- Produces `Options[java.time.MonthDay]`. The input must be a month-day in the ISO-8601 format `--MM-dd`, such as `--12-03`.
```scala mdoc:silent
Options.monthDay(name)
```
- Produces `Options[java.time.OffsetDateTime]`. The input must be a date-time with an offset from UTC/Greenwich in the ISO-8601 format, such as 2007-12-03T10:15:30+01:00.
```scala mdoc:silent
Options.offsetDateTime(name)
```
- Produces `Options[java.time.OffsetTime]`. The input must be a time with an offset from UTC/Greenwich in the ISO-8601 format, such as 10:15:30+01:00.
```scala mdoc:silent
Options.offsetTime(name)
```
- Produces `Options[java.time.Period]`. The input must be a date-based amount of time in the ISO-8601 format, such as 'P1Y2M3D'.
```scala mdoc:silent
Options.period(name)
```
- Produces `Options[java.time.Year]`. The input must be a year in the ISO-8601 format, such as 2007.
```scala mdoc:silent
Options.year(name)
```
- Produces `Options[java.time.YearMonth]`. The input must be a year-month in the ISO-8601 format, such as 2007-12.
```scala mdoc:silent
Options.yearMonth(name)
```
- Produces `Options[java.time.ZonedDateTime]`. The input must be an ISO-8601 offset date-time followed by the zone ID in square brackets, such as `2007-12-03T10:15:30+01:00[Europe/Paris]`.
```scala mdoc:silent
Options.zonedDateTime(name)
```
- Produces `Options[java.time.ZoneId]`. The input must be a time-zone ID, such as Europe/Paris.
```scala mdoc:silent
Options.zoneId(name)
```
- Produces `Options[java.time.ZoneOffset]`. The input must be a time-zone offset from Greenwich/UTC, such as +02:00.
```scala mdoc:silent
Options.zoneOffset(name)
```

### Empty options
`Options.none` produces an `Options[Unit]` that consumes no input. Use it for a command that takes no options. It is the same as leaving the options out of a command.
```scala mdoc:silent
Command("status", Options.none, Args.none)
```

### Combining and transforming options
We can use the following methods to create more complex Options:
```scala mdoc:silent
trait Options[A] {
  def ++[A1 >: A, B](that: Options[B]): Options[(A1, B)]
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

### Adding Options
Operator `++` can be used to zip two options. It can be used to chain two options in a tuple. For example, `git checkout` command has options `--quiet` and `--force`. The options for this command can be created in the following manner:
```scala mdoc:silent
val checkoutOptions = Options.boolean("quiet") ++ Options.boolean("force")
```

The output of the CLI help will be:
```
COMMANDS

  checkout [--quiet] [--force] 
```

### Choosing between Options
If we have more than one option and we need only one, we can create a new `Options` that allows to choose between two options using methods `|`, `orElse` and `orElseEither`. The difference between these three methods lies in the type parameter of the new `Options`. The user will be able to enter only the `Options` that he wants, triggering the desired functionality.
- Method `orElse`

`|` is just an alias for `orElse`. In the Git CLI, it is possible to choose between different options. This can be realized using `orElse`.
```scala mdoc:silent:reset
import zio.cli._
import java.nio.file.Path

val all = Options.boolean("all")
val interactive = Options.boolean("interactive")
val patch = Options.boolean("patch")

val orElseOption: Options[Boolean] = all | interactive | patch
```

- Method `orElseEither`

This method wraps the types in an `Either` class. This will be preferred if the types are very different.

```scala mdoc:silent
val orElseEitherOption: Options[Either[Either[Boolean, Boolean], Boolean]] = all orElseEither interactive orElseEither patch
```
This is another way to implement the `[--all | --interactive | --patch]` option of command `git commit`.


### Transforming Options
- Method `map`

It allows to transform the type parameter of `Options[A]`. It takes a function `f: A => B` as parameter that is applied when processing the input of a user in a CLI app. This makes easier to implement the business logic of a CLI app. This allows to implement `[--all | --interactive | --patch]` option of command `git commit` using `orElse`. This is not possible without method `map`, because the result of using `orElse` is an option that returns true if some of the options are employed.
```scala mdoc:silent:reset
import zio.cli._

sealed trait Mode
case class All() extends Mode
case class Interactive() extends Mode
case class Patch() extends Mode

val all = Options.boolean("all").map(_ => All())
val interactive = Options.boolean("interactive").map(_ => Interactive())
val patch = Options.boolean("patch").map(_ => Patch())

val alternative: Options[Mode] = all | interactive | patch
```

- Method `optional`

It wraps the option in an `Option` class. The resulting `Options` is now optional, so the user can choose to specify it or not. If it is specified, it will be wrapped in `Some()`. Without input from the user, it will produce `None`. For example, if we are designing a command-line application to manage databases, we might desire to retrieve data from a particular database. The user could specify an additional backup database. If there were an error connecting with the main database, the CLI app would try to retrieve data from the backup database.
```scala mdoc:silent
val database = Options.file("database")
val backup = Options.file("backup").optional
val retrieve = Command("retrieve", database ++ backup)
```
The HelpDoc of `optionsDatabase` will be 
```
COMMANDS

  retrieve --database <file> [--backup <file>]  
```
The `[]` denotes that the parameter is optional.

- Method `withDefault`

It creates an option that will have a default parameter in case that the user does not input the option. It can be used, for example, to specify a default directory in which a CLI application may perform some task if the user does not specify another one.
```scala mdoc:silent
val directory = Options.directory("directory").withDefault(java.nio.file.Paths.get("C:\\Users\\YourUser\\Documents"))
```
The output produced by the HelpDoc is
```
OPTIONS

  --directory <directory>
    A directory.

    This setting is optional. Default: 'C:\Users\YourUser\Documents'.
```

### Converting a tuple with as
If a combined option produces a tuple, `as` turns the tuple into a value of a case class or any other function of two to six arguments. It is a shorthand for `map` that spreads the tuple into the function's arguments.
```scala mdoc:silent
final case class Bounds(from: BigInt, to: BigInt)

val bounds: Options[Bounds] = (Options.integer("from") ++ Options.integer("to")).as(Bounds.apply _)
```

### Choosing between alternatives with fold
If an option produces an `Either`, for example after `|` between two options of different types, `fold` handles each side. It takes one function per side. For two sides, the first function handles `Left` and the second handles `Right`. Versions with two to six functions exist, and with three or more sides the nested `Either` is read from the left.
```scala mdoc:silent
val nameOrId: Options[String] =
  (Options.text("name").map(Left(_)) | Options.integer("id").map(Right(_)))
    .fold((name: String) => s"name $name", (id: BigInt) => s"id $id")
```

### Rejecting values with collect and mapOrFail
`collect` keeps the values that a partial function matches and rejects the others. The message is shown as the validation error.
```scala mdoc:silent
val colorCode: Options[Int] =
  Options.text("color").collect("color must be red or blue") {
    case "red"  => 1
    case "blue" => 2
  }
```
`mapOrFail` is the general form. The function returns `Left` with a `ValidationError` to reject a value, or `Right` with the new value to accept it.
```scala mdoc:silent
val port: Options[Int] =
  Options.integer("port").mapOrFail { (value: BigInt) =>
    if (value >= 1 && value <= 65535) Right(value.toInt)
    else Left(ValidationError(ValidationErrorType.InvalidValue, HelpDoc.p("Port must be between 1 and 65535.")))
  }
```

### Changing the placeholder with withPseudoName
The help output shows a placeholder after the option name, such as `<text>`. `withPseudoName` replaces it. It changes only the help text and does not change what the option accepts.
```scala mdoc:silent
Options.text("lastname").withDefault("xyz").withPseudoName("NAME")
```
The help output for this option is:
```
--lastname <NAME>
  A user-defined piece of text.

  This setting is optional. Default: 'xyz'.
```

### Making an alias
Using method `alias` it is possible to give a shorter form of the name of the `Options`. This is used to make easier the input of options for the user. We will make an alias for option `--directory`. 
```scala mdoc:silent
val directoryWithAlias = directory.alias("d")
```
The output produced by the HelpDoc in this case is
```
OPTIONS

  (-d, --directory <directory>)
    A directory.

    This setting is optional. Default: 'C:\Users\YourUser\Documents'.
```

### Adding help
Method `??` allows to add information about an options. The string is added after the current `HelpDoc` of the `Options`. We are going to recreate the `--quiet` option of `git checkout` to observe the effect of using `??`.
```scala mdoc:silent
val quiet = Options.boolean("quiet")
  
/* HelpDoc of quiet:
 *
 * --quiet
 *   A true or false value.
 * 
 *   This setting is optional. Default: 'false'.
 */

```
Now we add a description of the option:

```scala mdoc:silent
val quietWithHelp = quiet ?? "Suppress feedback messages."

/* HelpDoc of quietWithHelp:
 *
 * --quiet
 *   A true or false value.
 * 
 *   Suppress feedback messages.
 * 
 *   This setting is optional. Default: 'false'.
 */
```



### Example
As an example, we are going to construct an instance of `Options[Git]` that describes the options of the command `git commit`. We are going to implement only an option `--msg` that includes a commit message and alternatives between `-a`, `--interactive` and `--patch`. First, we create the basic options. Observe how we add an alias to `all`and `msg`.
```scala mdoc:silent:reset
import zio.cli._

val all = Options.boolean("all").alias("a")
val interactive = Options.boolean("interactive")
val patch = Options.boolean("patch")

val msg = Options.text("msg").alias("m")
```
Then we combine options:
- using `orElse` to combine `all`, `interactive` and `patch`,
- adding it to `msg`.

```scala mdoc:silent

val alternative = all | interactive | patch

val options = alternative ++ msg
```
The output of the HelpDoc of `options` is:
```
USAGE

  $ commit [(-a, --all)]|[--interactive]|[--patch] (-m, --msg <text>)

OPTIONS

  (-a, --all)
    A true or false value.

    This setting is optional. Default: 'false'.

  --interactive
    A true or false value.

    This setting is optional. Default: 'false'.

  --patch
    A true or false value.

    This setting is optional. Default: 'false'.

  (-m, --msg <text>)
    A user-defined piece of text.
```

### Platform support
Not every option constructor is available on every platform.
- `Options.oauth2` is available only on the JVM. See [Authorization with OAuth2](auth.md).
- `Options.file` is available on the JVM, Scala.js and Native.
- `Options.directory` is available on the JVM and Native, but not on Scala.js.

The matching argument constructors are described in [Platform support](args.md#platform-support).
