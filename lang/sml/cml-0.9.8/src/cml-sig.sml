(* cml-sig.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The user-level signature for the core CML module
 *)

signature CONCUR_ML =
  sig

    val version : {major : int, minor : int, rev : int, date : string}
    val versionName : string

  (** events **)
    type 'a event

    val sync   : 'a event -> 'a
    val select : 'a event list -> 'a
    val poll   : 'a event -> 'a option

    val choose : 'a event list -> 'a event

    val guard : (unit -> 'a event) -> 'a event

    val wrap        : ('a event * ('a -> 'b)) -> 'b event
    val wrapHandler : ('a event * (exn -> 'a)) -> 'a event
    val wrapAbort   : ('a event * (unit -> unit)) -> 'a event

    val always : 'a -> 'a event
    val ALWAYS : unit event (** for backward compatibility **)

  (** threads **)
    type thread_id

    val spawn : (unit -> unit) -> thread_id

    val yield : unit -> unit
    val exit : unit -> 'a

    val getTid : unit -> thread_id
    val sameThread : (thread_id * thread_id) -> bool
    val tidLessThan : (thread_id * thread_id) -> bool
    val tidToString : thread_id -> string

    val threadWait : thread_id -> unit event

  (** condition variables **)
    type 'a cond_var

    val condVar : unit -> '1a cond_var

    val writeVar : ('a cond_var * 'a) -> unit
    exception WriteTwice

    val readVar : 'a cond_var -> 'a
    val readVarEvt : 'a cond_var -> 'a event

  (** channels **)
    type 'a chan

    val channel : unit -> '1a chan

    val send   : ('a chan * 'a) -> unit
    val sendc  : 'a chan -> 'a -> unit
    val accept : 'a chan -> 'a

    val sameChannel : ('a chan * 'a chan) -> bool

    val transmit  : ('a chan * 'a) -> unit event
    val transmitc : 'a chan -> 'a -> unit event
    val receive   : 'a chan -> 'a event

  (** real-time synchronization **)
    datatype time = TIME of {sec : int, usec : int}  (* from System.Timer *)
	sharing type time = System.Timer.time
    val waitUntil : time -> unit event
    val timeout   : time -> unit event

  (* low-level I/O support (not for general use) *)
    exception InvalidFileDesc of int
    val syncOnInput  : int -> unit event
    val syncOnOutput : int -> unit event
    val syncOnExcept : int -> unit event

  end (* signature CONCUR_ML *)


(** The internal signature for the core CML module **)
signature INTERNAL_CML =
  sig

    structure CMLBase : CML_BASE

    include CONCUR_ML

    val initCML : unit -> unit
    val errCh : (thread_id * exn) chan
    val resetChan : 'a chan -> unit

  end; (* INTERNAL_CML *)
