(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature QUEUE =
    sig
	type '1a t
	exception Deq
	val create : unit -> '1a t
	val enq : 'a t -> 'a -> unit
	val deq : 'a t -> 'a
	val len : 'a t -> int
	val contents : 'a t -> 'a list
    end
