(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Greg Morrisett (Greg.Morrisett@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure SortableQueue :SORTABLE_QUEUE =
    struct
	datatype 'a queue = Q of ('a list ref * 'a list ref)

	fun empty () = Q (ref [], ref [])

	fun newq x = Q (ref [x], ref [])

	fun enq x (q as (Q(front,rear))) =
	    (rear := (x :: (!rear)); q)

	exception Deq

	fun deq (q as (Q(front,rear))) =
	    (case (!front) of
		 (x::rest) => (front := rest; q)
	       | [] => (case (!rear) of
			    [] => raise Deq
			  | r  => (front := rev r;
				   rear := [];
				   deq q)))

	fun head (q as (Q(front, rear))) =
	    (case (!front) of
		 (f as (x::rest)) => SOME(x)
	       | []  => (case (!rear) of
			     [] => NONE
			   | r  => (front := rev r;
				    rear := [];
				    head q)))

	fun sortq lt (q as (Q(front, rear))) =
	    let val l = !front @ (rev (!rear))
		fun insert x [] = [x]
		  | insert x (z as (hd::tl)) = 
		    (if (lt(x,hd)) then (x::z)
		     else (hd::(insert x tl)))
		fun sort [] res = res
		  | sort (hd::tl) partial_res =
		    sort tl (insert hd partial_res)
	    in
		front := (sort l []);
		rear := [];
		q
	    end
    end
	
