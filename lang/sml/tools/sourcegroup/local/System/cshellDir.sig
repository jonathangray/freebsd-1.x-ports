(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature CSHELLDIR = sig
  val cd : string -> unit
  val getwd : unit -> string
  val pwd : unit -> unit
  val dirs : unit -> unit
  val popd : unit -> unit
  val pushd : string -> unit
  val swapd : unit -> unit
  val homeDir :unit -> string
end
