(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature NAMES = sig
  type names
  val make  :string list * string list * string list -> names
  val break :names -> string list * string list * string list
end
