(* Copyright (c) 1992 by Carnegie Mellon University *)

functor NoOpToolDefFun () :TOOLDEF = struct

val toolName = "noOp"

val processor = ref ""
val targetNamer = ref (fn (pathname:string) => pathname)

fun loadSource (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  ()

fun genTarget (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  ()

fun loadTarget (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  ()

fun compileSource (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  ()

fun checkLoad (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  ()

fun validTarget (group:SourceGroup.group) (sourceName:string) (targetName:string) =
  true

end
