---
id: config-files
title: "Configuration Files"
---

ZIO CLI can load default option values from dotfiles named `.<app-name>`.

For an app named `wc`, ZIO CLI will look for `.wc` in:

1. The user's home directory.
2. The current directory's parent chain (from root down to the current working directory).

Priority is:

1. Command-line arguments (highest)
2. Current working directory dotfile
3. Parent directory dotfiles
4. Home directory dotfile (lowest)

If the same option appears in multiple files, the highest-priority source wins.
If the same option appears on the CLI, the CLI value wins.

Supported line formats in config files:

- `--key=value`
- `--key value`
- `--flag`

Blank lines and lines starting with `#` are ignored.

When configuration is applied, ZIO CLI prints the resolved values and their source files.
