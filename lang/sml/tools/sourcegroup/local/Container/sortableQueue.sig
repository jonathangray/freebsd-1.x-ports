(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature SORTABLE_QUEUE =
    sig
	type 'a queue
	val empty : unit -> '1a queue
	val newq  : '1a -> '1a queue
	val enq   : 'a -> 'a queue -> 'a queue
	val head  : 'a queue -> 'a option
	val deq   : 'a queue -> 'a queue
	val sortq : (('a * 'a) -> bool) -> 'a queue -> 'a queue
    end
