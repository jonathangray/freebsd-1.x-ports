(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ModuleDeclsFun (ModuleNames :MODULE_NAMES) :MODULE_DECLS = struct

structure MN = ModuleNames

datatype decl =
   ModDecl of {name:MN.moduleName,def:modExp,constraint:modExp option} list    
 | LocalDecl of decl * decl
 | SeqDecl of decl list    
 | OpenDecl of MN.moduleName list
 | DeclRef of MN.moduleName list

and modExp = 
   VarModExp of MN.moduleName
 | StructModExp of decl   
 | AppModExp of MN.moduleName * (modExp * bool) list
 | LetModExp of decl * modExp  
 | FctModExp of {params:(MN.moduleName option * modExp) list, body:modExp} 

datatype moduleInfo =
    MI of {def:modExp option, constraint:modExp option}

val emptyInfo = (MI {def=NONE, constraint=NONE})

end
