(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

structure Set :SET =
    struct
	fun member (e, set) = exists (fn x => x = e) set

	fun adjoin (e, x) =
	    if member (e, x) then
		x
	    else
		e :: x

	fun subset ([], y) = true
	  | subset (e :: rest, y) = member (e, y) andalso subset (rest, y)

	fun equal (x, y) = subset (x, y) andalso subset (y, x)

	fun union ([], y) = y
	  | union (e :: rest, y) =
	    if member (e, y) then
		union (rest, y)
	    else
		e :: union (rest, y)

	fun intersection ([], y) = []
	  | intersection (e :: rest, y) =
	    if member (e, y) then
		e :: intersection (rest, y)
	    else
		intersection (rest, y)

	fun difference ([], y) = []
	  | difference (e :: rest, y) =
	    if member (e, y) then
		difference (rest, y)
	    else
		e :: difference (rest, y)
    end
