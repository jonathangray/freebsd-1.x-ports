(* Copyright 1989 by AT&T Bell Laboratories *)

signature PPDEC =
sig
  type object
  val ppDec : Modules.env -> PrettyPrint.ppstream -> Absyn.dec
      -> (int -> object) -> unit
end
