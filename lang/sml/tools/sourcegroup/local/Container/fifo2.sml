(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Fifo2 :FIFO2 =
    struct
        datatype 'a t = Q of {front: 'a list, rear: 'a list}

	exception Deq

        val empty = Q{front=nil,rear=nil}

	fun enq (Q{front=f,rear=r}) x = Q{front=f,rear=(x::r)}

	fun deq (Q{front=(hd::tl),rear=r}) = (hd,Q{front=tl,rear=r})
	  | deq (Q{front=nil,rear=nil}) = raise Deq
	  | deq (Q{front=nil,rear=r}) = deq(Q{front=rev r,rear=nil})

	fun len (Q {rear,front}) = length (rear) + length (front)

	fun contents (Q {rear, front}) = (front @ (rev rear))

    end

