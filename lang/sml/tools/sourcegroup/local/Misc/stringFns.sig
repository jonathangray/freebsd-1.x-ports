(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature STRING_FNS =
    sig
	val escape : string -> string
	val increment : string -> string
	val gensym : string -> string
    end
