(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature EXECUTE = sig
  val firstLine :string * string list -> string
  val runAndPrint :string * string list -> unit
 end
