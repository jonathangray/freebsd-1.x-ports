(* Copyright (c) 1992 by Carnegie Mellon University *)

signature TRAVERSE = sig
  structure MD :MODULE_DECLS
  val traverse :System.Ast.dec -> MD.decl
end
