(* Copyright 1990 by AT&T Bell Laboratories *)
(* stamps.sig *)

signature STAMPS =
sig

  eqtype stamp
  type scope
  type 'a stampMap

  val freeScope : scope
  val newBoundScope : unit -> scope
  val newStamp : scope -> unit -> stamp

  (* newGenStamp: create a generative stamp, which when imported
     is guaranteed to be different from other stamps.  Used to
     create stamps for signatures.*)

  val newGenStamp : scope -> unit -> stamp

  val isBound : scope -> stamp -> bool
  val less : stamp * stamp -> bool
  val greater : stamp * stamp -> bool

  (* for convenience ... *)
  val newFree : unit -> stamp
  
  val error : stamp
  val null : stamp

  (* stamp maps should be changed to take the scope as an argument also.*)

  val newMap : exn -> '1a stampMap

  (* updateMap - add mapping to a stampMap *)
  val updateMap : '2a stampMap -> stamp * '2a -> unit

  (* applyMap - apply stampMap to a stamp *)
  val applyMap : 'a stampMap * stamp -> 'a

  val stampToString : stamp -> string

end (* STAMPS *)
