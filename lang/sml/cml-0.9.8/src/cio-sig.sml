(* cio-sig.sml
 *
 * COPYRIGHT (c) 1990,1991,1992 by John H. Reppy.  See COPYRIGHT file for details.
 *)

signature CONCUR_IO =
  sig
    structure CML : CONCUR_ML

    exception Io of string

    type instream
    type outstream

    val std_in : instream
    val std_out : outstream
    val std_err : outstream

    val open_in : string -> instream
    val open_string : string -> instream
    val open_out : string -> outstream
    val open_append : string -> outstream
    val execute : (string * string list) -> (instream * outstream)
    val execute_in_env : (string * string list * string list)
	  -> (instream * outstream)

    exception ClosedStream
    val openChanIn  : unit -> ((string -> unit) * instream)
    val openChanOut : unit -> (string CML.event * outstream)

    val close_in : instream -> unit
    val close_out : outstream -> unit

    val can_input : instream -> int
    val lookahead : instream -> string
    val input : instream * int -> string
    val inputc : instream -> int -> string
    val input_line : instream -> string
    val end_of_stream : instream -> bool

    val lookaheadEvt : instream -> string CML.event
    val inputEvt : instream * int -> string CML.event
    val inputcEvt : instream -> int -> string CML.event
    val inputLineEvt : instream -> string CML.event

    val output : outstream * string -> unit
    val outputc : outstream -> string -> unit
    val flush_out : outstream -> unit

    val print : string -> unit

  end (* CONCUR_IO *)
