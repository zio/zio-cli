---
id: how-to-package-native-binary
title: "How to package a native binary"
---

Build a native executable of your CLI with the ZIO CLI sbt plugin.

The plugin has one task that works: `zioCliBuildNative`. It builds a native image of the main class you set. The other plugin tasks are not implemented yet. They print `TODO: Not Implemented!` and do nothing:

- `zioCliInstallCli`
- `zioCliGenerateBashCompletion`
- `zioCliGenerateZshCompletion`

The plugin does not create an installer or a package file. The output is the native executable.

You need:
- An sbt project with a `main` method in a `ZIOCliDefault` object (see [Bash completion](how-to-add-shell-completion.md) for an example app).
- The `sbt-zio-cli` plugin published locally. Follow [ZIO CLI SBT Plugin](sbt-plugin.md) to run `publishLocal` on the `sbtZioCli` project.
- A GraalVM distribution with its `native-image` tool, which the build runs. This repository does not document how to install it. See the [sbt-native-image](https://github.com/scalameta/sbt-native-image) README.

## Steps

1. Add the plugin to `project/plugins.sbt`:

```
addSbtPlugin("zio.cli.sbt" % "sbt-zio-cli" % "0.0.0-SNAPSHOT")
```

2. Enable the plugin in `build.sbt` and set the main class. `zioCliMainClass` defaults to `None`, so you must set it. Use the fully qualified name of your object:

```
import zio.cli.sbt.ZIOCLIPlugin

lazy val root = project
  .in(file("."))
  .enablePlugins(ZIOCLIPlugin)
  .settings(
    zioCliMainClass := Some("zio.cli.YourApp")
  )
```

3. Build the native executable from sbt:

```
sbt> zioCliBuildNative
```

   The task sets `Compile / mainClass` from `zioCliMainClass`, then runs the `nativeImage` task of `sbt-native-image`. When the image is ready, the task calls `zioCliNativeImageReady`, which prints `ZIO CLI App Native Image Ready!` by default.

4. Find the executable. The output location is set by `sbt-native-image`. This guide does not verify the path. Check the output of the build and the `sbt-native-image` README.

## Settings you can change

- `zioCliNativeImageOptions` is a list of extra arguments for `native-image`. Its default is `--allow-incomplete-classpath`, `--report-unsupported-elements-at-runtime`, `--initialize-at-build-time`, and `--no-fallback`. Replacing it changes what is passed to `native-image`.
- `zioCliNativeImageReady` is a callback with type `() => Unit`. It runs after a successful build.

See [ZIO CLI SBT Plugin](sbt-plugin.md) for the full list of settings and tasks.

## See also

- [ZIO CLI SBT Plugin](sbt-plugin.md): setup, settings, and the status of each task.
- [How to add shell completion (bash)](how-to-add-shell-completion.md): completion through the built-in options. The plugin's completion task is not implemented.
