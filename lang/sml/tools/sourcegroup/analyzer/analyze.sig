(* Copyright (c) 1992 by Carnegie Mellon University *)

signature ANALYZE = sig
  val connections :string ->
        (string list * string list * string list * string list) *
        (string list * string list * string list * string list)
  val report :string -> unit
  val scan :string -> unit
  val test :unit -> unit
  val interface :string list * string list -> unit
end
