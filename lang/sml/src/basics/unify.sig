(* Copyright 1990 by AT&T Bell Laboratories *)

signature UNIFY =
sig

  (* type unification *)
  exception Unify of string
  val unifyTy : Types.ty * Types.ty -> unit

end
