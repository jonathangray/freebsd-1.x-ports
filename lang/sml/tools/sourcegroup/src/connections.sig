(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

signature CONNECTIONS = sig
  exception ConnectionsError
  structure NameRefTable :NAMEREFTABLE
  val get :string -> (string * string * NameRefTable.t * NameRefTable.t) list
end

(* normalize all name lists *)
