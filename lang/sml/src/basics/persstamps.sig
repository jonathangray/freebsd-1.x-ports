(* Copyright 1992 by AT&T Bell Laboratories *)
(* persstamps.sig *)

signature PERSSTAMPS =
sig

  eqtype persstamp
  type 'a stampMap

  (* generate new persistent stamp, unique over all time
   * for a given machine class. *)
  val newStamp : unit -> persstamp

  (* total ordering on persstamps *)
  val less : persstamp * persstamp -> bool
  val greater : persstamp * persstamp -> bool

  (* create a new stampMap for persistent stamps;
   * raise given exception in applyMap if stamp 
   * is not in the stampMap
   *)
  val newMap : exn -> '1a stampMap

  (* updateMap - add mapping to a stampMap *)
  val updateMap : '2a stampMap -> persstamp * '2a -> unit

  (* applyMap - apply stampMap to a stamp *)
  val applyMap : 'a stampMap * persstamp -> 'a

  val stampToString : persstamp -> string

end (* PERSSTAMPS *)
