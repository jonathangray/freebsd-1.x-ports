(* Copyright (c) 1992 by Carnegie Mellon University *)

signature SOURCE_GROUP_EXPERT = sig
  type group

  datatype sourceInfo = Source of
      {sourceName     :string,
       targetName     :string,
       group          :group,
       toolName       :string,
       envCurrent     :bool,
       targetCurrent  :bool,
       dependsOn      :string list,
       loadSource     :unit -> unit,
       genTarget      :unit -> unit,
       loadTarget     :unit -> unit,
       checkLoad      :unit -> unit,
       compileSource  :unit -> unit}

  val makeAll       :(sourceInfo -> unit) -> group -> unit
  val makeOnDemand  :(sourceInfo -> unit) -> group -> string -> unit
  val connections   :string -> group -> unit
  val connections'  :outstream -> group -> unit
  val dependsOn     :string -> string list

  val issueWarnings :bool ref
  val quietCreation :bool ref
  val continueAfterError :bool ref

  structure ToolInternals :TOOL_INTERNALS
end


signature SOURCEGROUP = sig
 val version :real
 type group
 exception CompilingError

 datatype groupDescription =
    Sources of string list
  | SubGroups of group list
  | Connections of string list

 val createInEnv :environment -> groupDescription list -> group
 val create      :groupDescription list -> group
 val make        :group -> string -> unit
 val makeWhat    :group -> string -> unit

 structure Expert :SOURCE_GROUP_EXPERT sharing type group = Expert.group
end
