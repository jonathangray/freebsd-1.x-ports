(* ex-buffer-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is the signature of unbounded buffered channels.  Send operations never
 * block, but accept/receive operations may.
 *)

(* BEGIN EXAMPLE *)
signature BUFFER_CHAN =
  sig
    structure CML : CONCUR_ML
    type 'a buffer_chan
    val buffer : unit -> '1a buffer_chan
    val bufferSend : ('a buffer_chan * 'a) -> unit
    val bufferAccept : 'a buffer_chan -> 'a
    val bufferReceive : 'a buffer_chan -> 'a CML.event
  end (* BUFFER_CHAN *)
(* END EXAMPLE *)
