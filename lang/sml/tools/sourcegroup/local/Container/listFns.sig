(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature LIST_FNS =
    sig
	exception Zip
	val zip : 'a list -> 'b list -> ('a * 'b) list
	val unzip : ('a * 'b) list -> 'a list * 'b list

	exception NotFound
	val find : ('a -> bool) -> 'a list -> 'a
	val assoc : ''a -> (''a * 'b) list -> 'b
	val index : ''a -> ''a list -> int

	exception Split
	val split : int -> 'a list -> 'a list * 'a list

	(* list_string init sep fin format_fn list
	   uses the init, sep, and fin strings and the format function
	   to convert the list to a string. *)

	val list_string : string -> string -> string ->
                            ('a -> string) -> 'a list -> string

	val filter : 'a option list -> 'a list
    end
