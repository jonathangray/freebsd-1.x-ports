(* Copyright (c) 1992 by Carnegie Mellon University *)

functor DefineTool (ToolDef:TOOLDEF) :TOOL = struct

open ToolDef

structure ToolInternals = SourceGroup.Expert.ToolInternals

fun targetNameOf (sourceName:string) = (!targetNamer) sourceName

val _ = ToolInternals.defineTool (ToolInternals.Tool
 {toolName = toolName,
  targetNameOf = targetNameOf,
  targetNamer = targetNamer,
  loadSource = loadSource,
  genTarget = genTarget,
  loadTarget = loadTarget,
  compileSource = compileSource,
  validTarget = validTarget,
  checkLoad = checkLoad})

end
