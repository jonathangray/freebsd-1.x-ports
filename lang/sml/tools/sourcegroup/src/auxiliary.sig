(* Copyright (c) 1992 by Carnegie Mellon University *)

signature AUXILIARY = sig
  structure Data :DATA
  val circle :Data.groupInfo -> string list -> unit
  val connections  :string -> Data.group -> unit
  val connections' :outstream -> Data.group -> unit
  val arrangeFiles :string list -> (string list * string list * string list)
end
