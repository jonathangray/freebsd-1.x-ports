(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Externally visible aspects of the lexer and parser *)

signature INTERFACE = sig

type pos
val line : pos ref
val initLine : unit -> unit
val nextLine : unit -> unit
val makeString :pos -> string

end  (* signature INTERFACE *)
