(* run-cml-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Code to support top-level interactive use of CML.
 *)

signature RUN_CML =
  sig

    structure CML : CONCUR_ML

  (* log/unlog channels and servers for initialization and termination *)
    exception Unlog
    val logChannel : (string * 'a CML.chan) -> unit
    val unlogChannel : string -> unit
    val logServer : (string * (unit -> unit) * (unit -> unit)) -> unit
    val unlogServer : string -> unit
    val unlogAll : unit -> unit

  (* run the system *)
    val doit : ((unit -> unit) * int option) -> unit
    exception Running

  (* export a CML program *)
    val exportFn : (string * ((string list * string list) -> unit) * int option)
	  -> unit

  (* shutdown a run *)
    val shutdown : unit -> 'a
    exception NotRunning

  end; (* RUN_CML *)
