(* Copyright 1989 by AT&T Bell Laboratories *)
(* overload.sig *)

signature OVERLOAD =
sig
(*   structure Env:ENV *)
  val resetOverloaded : unit -> unit
  val pushOverloaded : Variables.var ref * ErrorMsg.complainer -> Types.ty
  val resolveOverloaded : Modules.env -> unit
end  (* signature OVERLOAD *)
