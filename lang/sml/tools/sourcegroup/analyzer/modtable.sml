(* Copyright (c) 1992 by Carnegie Mellon University *)

structure ModTable = struct

structure MD = ModuleDecls
structure MN = ModuleDecls.MN

val hasher = Hasher.hasher

datatype moduleInfo =
    MI of {def:MD.modExp option, constraint:MD.modExp option}
type moduleTable = (string, moduleInfo) Hash.table
type modtable = moduleTable array

fun empty () :modtable = Hash.createDefault ([]:moduleInfo list)

fun create () :modtable =
  let val tbl :modtable =  Array.array (4, empty()) in
    Array.update (moduleTable, 1, empty());
    Array.update (moduleTable, 2, empty());
    Array.update (moduleTable, 3, empty());
    tbl
  end

fun lookup (modtable:modtable)
           (kind:MN.moduleKind, nameH:string*int) :moduleInfo option =
  Hash.lookup (Array.sub (modtable, MN.ordOf kind)) nameH

fun enter (modtable:modtable)
          (kind:MN.moduleKind, nameH:string*int, info:moduleInfo option):unit=
  Hash.enter (Array.sub (modtable, MN.ordOf kind)) nameH info

val emptyInfo = (MI {def=NONE, constraint=NONE})

end
