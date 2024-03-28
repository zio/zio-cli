addSbtPlugin("ch.epfl.scala"                     % "sbt-bloop"                     % "1.5.16")
addSbtPlugin("ch.epfl.scala"                     % "sbt-scalafix"                  % "0.11.0")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"                 % "0.11.0")
addSbtPlugin("com.github.sbt"                    % "sbt-unidoc"                    % "0.5.0")
addSbtPlugin("com.github.sbt"                    % "sbt-ci-release"                % "1.5.11")
addSbtPlugin("com.github.cb372"                  % "sbt-explicit-dependencies"     % "0.2.16")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"              % "3.0.0")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"                   % "1.14.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"              % "0.4.16")
addSbtPlugin("org.scalameta"                     % "sbt-mdoc"                      % "2.3.2")
addSbtPlugin("org.scalameta"                     % "sbt-scalafmt"                  % "2.5.2")
addSbtPlugin("org.scoverage"                     % "sbt-scoverage"                 % "2.0.9")
addSbtPlugin("pl.project13.scala"                % "sbt-jmh"                       % "0.4.6")
addSbtPlugin("org.scalameta"                     % "sbt-native-image"              % "0.3.2")
addSbtPlugin("dev.zio"                           % "zio-sbt-website"               % "0.3.6")

resolvers += Resolver.sonatypeRepo("public")
