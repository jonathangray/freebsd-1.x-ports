(* Copyright (c) 1992 by Carnegie Mellon University *)

signature MODULE_NAMES = sig
 datatype moduleKind = structMod | sigMod | functorMod | fsigMod
 eqtype moduleName

 val ordOf :moduleKind -> int

 val modNameString :moduleName -> string
 val modNameQualId :moduleName -> string
 val modKind       :moduleName -> moduleKind
 val headModule    :moduleName -> (moduleKind * string)

 val structPath  :symbol list -> moduleName
 val sigPath     :symbol list -> moduleName
 val functorPath :symbol list -> moduleName
 val fsigPath    :symbol list -> moduleName
 val modulePath  :symbol list -> moduleName

 val moduleKind :symbol -> moduleKind option

 val genericModuleName :unit -> moduleName
end
