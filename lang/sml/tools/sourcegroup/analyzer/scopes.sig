(* Copyright (c) 1992 by Carnegie Mellon University *)

signature SCOPES = sig
 structure MD :MODULE_DECLS

 type scope
 type modtable

 val sharedTable :unit -> modtable
 val lookupShared :modtable -> ((MD.MN.moduleKind*string)*int) ->
                     MD.moduleInfo option
 val enterShared :modtable -> ((MD.MN.moduleKind*string)*int) ->
                     MD.moduleInfo -> unit

 val sourceScope :unit -> scope
 val bindScope   :scope -> scope
 val localScope  :scope -> scope
 val plainScope  :scope -> scope
 val popScope    :scope -> scope
 val enterRef    :modtable -> scope -> MD.MN.moduleName -> MD.moduleInfo
 val enterDef    :scope -> MD.MN.moduleName * MD.moduleInfo -> unit

 val printAll   :outstream -> scope -> unit

 val connections :scope ->
       (string list * string list * string list * string list) *
       (string list * string list * string list * string list)
end
