(* ex-buffer-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

(* BEGIN EXAMPLE *)
signature UNIQUE_ID =
  sig
    eqtype id
    val nextId : unit -> id
  end (* UNIQUE_ID *)
(* END EXAMPLE *)
