(* Copyright (c) 1992 by Carnegie Mellon University *)

signature PROCESS = sig
  structure SC :SCOPES
  val processDecl :SC.modtable -> SC.MD.decl -> SC.scope
end
