(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature CONN = sig
  structure AbSyn :ABSYN
  val parse :bool -> string -> AbSyn.ast
end
