(* Copyright (c) 1992 by Carnegie Mellon University *)

signature TOOL_INTERNALS = sig
  structure Data :DATA
  datatype toolInfo = Tool of
    {toolName      :string,
     targetNameOf  :string -> string,
     targetNamer   :(string -> string) ref,
     validTarget   :Data.group -> string -> string -> bool,
     loadSource    :Data.group -> string -> string -> unit,
     genTarget     :Data.group -> string -> string -> unit,
     loadTarget    :Data.group -> string -> string -> unit,
     compileSource :Data.group -> string -> string -> unit,
     checkLoad     :Data.group -> string -> string -> unit}

  val defineTool    :toolInfo -> unit
  val getToolInfo   :string * int -> toolInfo
  val toolIsDefined :string * int -> bool
end
