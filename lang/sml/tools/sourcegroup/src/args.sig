(* Copyright (c) 1992 by Carnegie Mellon University *)

signature ARGS = sig
  structure Data :DATA
  val rearrange :Data.groupDescription list ->
                   string list * Data.groupInfo list * string list
end
