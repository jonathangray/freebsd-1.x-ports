(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure ListSort  :LISTSORT = struct

fun sort (compare :'a * 'a -> bool) ([]:'a list) :'a list = []
  | sort compare [a] = [a]
  | sort compare (lst as a::b::c) =
      let fun merge ([],[]) = []
	    | merge (lst as ((a::b) :'a list), []) = lst
	    | merge ([], (lst as (c::d) :'a list)) = lst
	    | merge (((a::b) :'a list), ((c::d) :'a list)) =
	        if compare(a,c) then a::(merge (b, (c::d)))
	          else c::(merge ((a::b), d))
          fun msort [lst] = [lst]
	    | msort [] = []
	    | msort (lst as x::y::z) = (merge(x,y))::(msort z);
	  fun mergesort [] = []
	    | mergesort [lst] = [lst]
	    | mergesort (lst as x::y::z) = mergesort (msort lst);
          fun makelist a = [a];
          val [newlist] = mergesort (map makelist lst)
      in newlist end;

fun unique'sort (equal :'a*'a->bool) (compare :'a*'a->bool) ([]:'a list)
                :'a list = []
  | unique'sort equal compare [a] = [a]
  | unique'sort equal compare (lst as a::b::c) =
      let fun merge ([],[]) = []
	    | merge (lst as ((a::b) :'a list), []) = lst
	    | merge ([], (lst as (c::d) :'a list)) = lst
	    | merge (((a::b) :'a list), ((c::d) :'a list)) =
                if equal(a,c) then merge (b, (c::d))
	          else if compare(a,c) then a::(merge (b, (c::d)))
                         else c::(merge ((a::b), d))
          fun msort [lst] = [lst]
	    | msort [] = []
	    | msort (lst as x::y::z) = (merge(x,y))::(msort z);
	  fun mergesort [] = []
	    | mergesort [lst] = [lst]
	    | mergesort (lst as x::y::z) = mergesort (msort lst);
          fun makelist a = [a];
          val [newlist] = mergesort (map makelist lst)
      in newlist end;

end
