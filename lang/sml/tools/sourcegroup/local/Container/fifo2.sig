(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature FIFO2 =
    sig
	type 'a t
	exception Deq
	val empty : 'a t
	val enq : 'a t -> 'a -> 'a t
	val deq : 'a t -> 'a * 'a t
	val len : 'a t -> int
	val contents : 'a t -> 'a list
    end
