(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature LISTSORT = sig
  val sort :('a * 'a -> bool) -> 'a list -> 'a list
  val unique'sort :('a * 'a -> bool) -> ('a * 'a -> bool) -> 'a list -> 'a list
end
