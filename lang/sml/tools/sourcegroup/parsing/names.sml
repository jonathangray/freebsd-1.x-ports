(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor NamesFun () :NAMES = struct
  type names = string list * string list * string list

  fun make (s0 :string list, s1 :string list, s2 :string list) :names =
        (s0, s1, s2)
  fun break ((s0, s1, s2) :names) :string list * string list * string list =
        (s0, s1, s2)
end
