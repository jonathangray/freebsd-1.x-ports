(* Copyright (c) 1992 by Carnegie Mellon University *)

structure SMLToolDef = struct

val toolName = "sml"

val processor = ref ""
val targetNamer = ref (fn (pathname:string) =>  (pathname^".bin"))

fun loadSource (group:SourceGroup.group) sourceName targetName =
  Compile.loadSource sourceName

fun genTarget (group:SourceGroup.group) sourceName targetName =
  Compile.genTarget (sourceName, targetName)

fun loadTarget (group:SourceGroup.group) sourceName targetName =
  Compile.loadTarget targetName

fun compileSource (group:SourceGroup.group) sourceName targetName =
  Compile.compile (sourceName, targetName)

fun validTarget (group:SourceGroup.group) sourceName targetName =
  Compile.validTarget targetName

fun checkLoad (group:SourceGroup.group) sourceName targetName =
  if validTarget group sourceName targetName
    then loadTarget group sourceName targetName
    else loadSource group sourceName targetName

end
