(* Copyright (c) 1992 by Carnegie Mellon University *)

(* for Standard SML/NJ, with no local compiler mods *)

functor SMLToolDefFun () :TOOLDEF = struct

type group = SourceGroup.group

val toolName = "sml"

val processor = ref ""
val targetNamer = ref (fn (pathname:string) =>  (pathname^".bin"))

fun err (msg:string) =
 (print ("? SML Tool: Operation " ^ msg ^
          " not available in this SML compiler\n");
  raise SourceGroup.CompilingError)

fun loadSource (group:group) (sourceName:string) (targetName:string) =
  IO.use sourceName

fun genTarget (group:group) (sourceName:string) (targetName:string) =
  err "genTarget"

fun loadTarget (group:group) (sourceName:string) (targetName:string) =
  err "loadTarget"

fun compileSource (group:group) (sourceName:string) (targetName:string) =
  err "compileSource"

fun checkLoad (group:group) (sourceName:string) (targetName:string) =
  loadSource group sourceName targetName

fun validTarget (group:group) (sourceName:string) (targetName:string) =
  false

end
