(* multicast-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

signature MULTICAST =
  sig
    structure CML : CONCUR_ML
    type 'a mchan
    val mChannel : unit -> '1a mchan
    val newPort : 'a mchan -> 'a CML.event
    val multicast : ('a mchan * 'a) -> unit
  end (* MULTICAST *)
