(* Copyright (c) 1992 by Carnegie Mellon University *)

signature MODTABLE = sig
  structure MD :MODULE_DECLS

  datatype moduleInfo =
    MI of {def:MD.modExp option, constraint:MD.modExp option}
  type modtable

  val create :unit -> modtable
  val lookup :modtable -> ((MD.MN.moduleKind*string)*int) -> moduleInfo option
  val enter  :modtable -> ((MD.MN.moduleKind*string)*int) * moduleInfo -> unit
  val emptyInfo :moduleInfo
end
