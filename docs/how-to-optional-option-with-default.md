---
id: how-to-optional-option-with-default
title: "How to make an option optional with a default"
---

Make a flag or option optional, so the user can leave it out, and choose what your program uses when it is left out.

Two methods do this:

- `.withDefault(value)` makes the option optional and uses `value` when it is left out.
- `.optional` makes the option optional and produces `None` when it is left out.

Boolean flags (`Options.boolean`) already have a default. Their default is the opposite of the value the flag gives when present.

You need:
- The `zio-cli` library on your classpath.
- A basic understanding of how options are built. See [Options](options.md).

## Steps

1. Create a boolean flag. A flag takes no value. `--quiet` gives `true`. If the flag is left out, the value is `false`:

```scala mdoc:silent
import zio.cli._

val quiet: Options[Boolean] = Options.boolean("quiet")
```

2. To invert a flag, pass `ifPresent = false`. The flag then gives `false` when present, and the default is `true` when it is left out:

```scala mdoc:silent
val progress: Options[Boolean] = Options.boolean("progress", ifPresent = false)
```

3. Give a non-boolean option a default with `.withDefault`. The default must have the type of the option. `Options.integer` produces `BigInt`, so convert it first with `.map(_.toInt)`:

```scala mdoc:silent
val retries: Options[Int] = Options.integer("retries").map(_.toInt).withDefault(3)
```

4. Make an option optional with `.optional`. The result type is `Option[A]`. If the user leaves the option out, the value is `None`:

```scala mdoc:silent
val user: Options[Option[String]] = Options.text("user").optional
```

5. Combine the options in a command with `++`. The command receives a tuple with one value per option, in the order you combined them:

```scala mdoc:silent
val serve = Command("serve", quiet ++ retries)
```

6. Check the help text. The help text of an option with `.withDefault` ends with this sentence, where `value` is the default:

   `This setting is optional. Default: 'value'.`

   An option with `.optional` gets only `This setting is optional.`, because its default is `None`. In the usage line, an optional option is shown in square brackets.

## See also

- [Options](options.md): how to build options, including `.optional` and `.withDefault`.
- [Commands](commands.md): how to combine options into a command.
