(* Copyright (c) 1992 by Carnegie Mellon University *)

signature PRETTY = sig
  structure MD :MODULE_DECLS
  val prettyPrint :string -> unit
  val showDir :string -> unit
  val pp_decl :MD.decl -> unit
end
