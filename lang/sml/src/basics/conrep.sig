(* Copyright 1989 by AT&T Bell Laboratories *)
(* conrep.sig *)

signature CONREP =
sig
  val boxed : (Symbol.symbol * bool * Types.ty) list -> Access.conrep list
end
