(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Drew Dean <rdd@cs.cmu.edu>
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15218 *)

signature GETENV = sig
  exception GetEnv
  val getenv   :string -> string
  val printenv :unit -> unit
end
