(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ToolInternalsFun
 (structure Data :DATA
  structure Util :UTIL
  structure DirFile :DIRFILE
 ) :TOOL_INTERNALS = struct

 structure Data = Data
 structure Hash = Data.Hash
 structure Hasher = Data.Hasher

open Data

datatype toolInfo = Tool of
  {toolName      :string,
   targetNameOf  :string -> string,
   targetNamer   :(string -> string) ref,
   validTarget   :group -> string -> string -> bool,
   loadSource    :group -> string -> string -> unit,
   genTarget     :group -> string -> string -> unit,
   loadTarget    :group -> string -> string -> unit,
   compileSource :group -> string -> string -> unit,
   checkLoad     :group -> string -> string -> unit}

exception Skip
exception SourceGroup'InternalError

val modtime = DirFile.timeModified

val toolTable = Hash.createDefault ([]:toolInfo list)

fun getToolInfo (toolH as (toolName,_)) = Hash.lookup' toolTable toolH

fun toolIsDefined (toolH as (toolName,_)) =
  case  Hash.lookup toolTable toolH of
     NONE => false | (SOME _) => true

fun defineTool (toolInfo:toolInfo as Tool {toolName,...}) =
  Hash.enter toolTable (Hasher.hasher toolName) toolInfo

val _ = (Util.toolIsDefined := toolIsDefined)
end
