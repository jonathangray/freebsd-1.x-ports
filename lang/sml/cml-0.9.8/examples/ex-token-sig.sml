(* ex-token-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * A simple token server.
 *)

(* BEGIN EXAMPLE *)
signature TOKEN_SERVER =
  sig
    structure CML : CONCUR_ML
    type ('a, 'b) token
    val newToken : ('a -> 'b) -> ('a, 'b) token
    exception NotTokenHolder
    val getOperation : ('a, 'b) token -> ('a -> 'b)
    val releaseToken : ('a, 'b) token -> unit
    val acquireToken : ('a, 'b) token -> unit CML.event
  end (* TOKEN_SERVER *)
(* END EXAMPLE *)
