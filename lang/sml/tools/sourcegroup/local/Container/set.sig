(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature SET =
    sig
	val member : ''a * ''a list -> bool
	val subset : ''a list * ''a list -> bool
	val equal : ''a list * ''a list -> bool

	val adjoin : ''a * ''a list -> ''a list
	val union : ''a list * ''a list -> ''a list
	val intersection : ''a list * ''a list -> ''a list
	val difference : ''a list * ''a list -> ''a list
    end
