(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Queue :QUEUE =
    struct
	type '1a t = '1a Fifo2.t ref

	exception Deq = Fifo2.Deq

	fun create () = ref Fifo2.empty

	fun enq q x = q := (Fifo2.enq (!q) x)

        fun deq q = 
	    let val (x,newq) = Fifo2.deq (!q) in
		q := newq;
		x
	    end
	
	fun len q = Fifo2.len (!q)

	fun contents q = Fifo2.contents (!q)

    end
