(* Copyright 1989 by AT&T Bell Laboratories *)
(* printval.sig *)

signature PPVAL = 
sig
  type object
  val ppVal : Modules.env -> PrettyPrint.ppstream -> object * Types.ty * int -> unit
end
