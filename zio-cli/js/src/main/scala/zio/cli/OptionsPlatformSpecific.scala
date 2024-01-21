package zio
package cli

private[cli] trait OptionsPlatformSpecific extends PathPlatformSpecific { self: Options.type => 
    
    /**
   * Creates a parameter expecting path to the file.
   */
  def file(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Single(name, Vector.empty, PrimType.Path(PathType.File, exists))
}
