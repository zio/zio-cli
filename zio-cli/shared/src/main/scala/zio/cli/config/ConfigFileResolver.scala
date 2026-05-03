package zio.cli.config

/**
 * Resolves dotfile-based configuration files for a CLI command.
 *
 * Searches for `.<commandName>` files in the following order (lowest to highest priority):
 *   1. Home directory
 *   2. Root directory → ... → parent directories
 *   3. Current working directory (highest priority)
 *
 * Platform-specific: JVM/Native use `java.nio.file`, JS returns empty.
 */
object ConfigFileResolver extends ConfigFileResolverPlatformSpecific
