(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ConfigureFun
 (structure Scopes  :SCOPES
  structure Process :PROCESS
 ) :CONFIGURE = struct

structure MD = Scopes.MD
structure MN = Scopes.MN

datatype moduleInfo =
   StrInfo of {def:MD.strExp, constraint:MD.sigExp option}
 | FctInfo of MD.fctExp
 | SigInfo of MD.sigExp
 | FsigInfo of MD.fsigExp

val modtable = (MN.moduleKind * string, moduleInfo) Hash.table

fun connections filename 
      :(string list * string list * string list * string list) *
       (string list * string list * string list * string list) =
  let val ast = Parse.parseSource filename
      val decl = Traverse.traverse ast
  in
    Process.processDecl decl;
    Scopes.connections ()
  end

fun enterModule 

fun configureFile filename =
  let val (_, (e_str, e_sig, e_fun, e_fsig)) = connections filename in
    

fun configure files = ()

end
