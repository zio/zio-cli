package zio.cli

import java.nio.file.Path

private[cli] trait PathPlatformSpecific {

  type JPath = Path

}

private[cli] object PathPlatformSpecific {

  type JPath = Path
  
}