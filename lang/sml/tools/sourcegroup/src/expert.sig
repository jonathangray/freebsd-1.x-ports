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
  val dependsOn     :group -> string -> string list

  val libraryAction :(sourceInfo -> unit) ref
  val makeLibraries :bool ref
  val issueWarnings :bool ref
  val quietCreation :bool ref
  val continueAfterError :bool ref
end
