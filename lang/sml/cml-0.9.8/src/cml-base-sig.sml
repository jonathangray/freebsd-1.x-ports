(* cml-base-sig.sml
 *)

signature CML_BASE =
  sig

    val version : {major : int, minor : int, rev : int, date : string}
    val versionName : string

    datatype 'a queue_t = Q of {front : 'a list ref, rear : 'a list ref}

  (* Per-thread descriptors *)
    datatype thread_id = TID of {  (* thread ids *)
	id	   : int,
	done_comm  : bool ref,		(* set this whenever this thread does *)
					(* some concurrency operation. *)
	death_cond : unit cond_var
      }
  (* condition variables *)
    and 'a cond_var = COND of 'a cond_state ref
    and 'a cond_state
      = COND_unset of (thread_id * bool ref * 'a cont) list
      | COND_set of 'a

    val dummyTId : thread_id

  (* The thread ready queues:
   * rdyQ1 is the primary queue and rdyQ2 is the secondary queue.
   *)
    val rdyQ1 : (thread_id * unit cont) queue_t
    val rdyQ2 : (thread_id * unit cont) queue_t

    val dequeue2 : unit -> (thread_id * unit cont)
	(* remove a thread from the secondary scheduling queue (assuming
	 * that the primary queue is empty).
	 *)

    datatype time = TIME of {sec : int, usec : int}
	sharing type time = System.Timer.time

    val currentTime : unit -> time

    val timerOff : unit -> unit
    val timerOn : time option -> unit
    val restartTimer : unit -> unit

    datatype io_operation_t = IO_RD | IO_WR | IO_EX

    val pollFDs : (int list * int list * int list * bool)
	  -> (int list * int list *int list)

    val insIOWait : {
	    fd : int, io_op : io_operation_t, kont : unit cont,
	    id : thread_id, err_kont : unit cont, dirty : bool ref
	  } -> unit
 
    val insTimeWait : (time * thread_id * unit cont * bool ref) -> unit

  (* global flag for implementing atomic operations *)
    datatype atomic_state = NonAtomic | Atomic | SignalPending
    val atomicState : atomic_state ref

    val checkWaitingThreads : unit -> unit

    val handlePendingSignal : unit -> unit
	(* Complete the exit from an atomic region when there is a
	 * pending signal.
	 *)

    val initCMLBase : unit -> unit

    exception InternalError

    val reportError : string -> unit
	(* atomically print a message on std_err *)

    val error : string -> 'a
	(* report an internal error on std_err, and raise InternalError *)

    val shutdown : (unit -> unit) ref
	(* the termination function *)

    val load : unit -> int
	(* return the number of threads on the ready queues *)

    val go : time option -> unit
	(* mark the beginning of CML execution and start the timer *)

    val stop : unit -> unit
	(* turn the timer off, and mark the end of CML execution *)

    val isRunning : unit -> bool
	(* returns true if CML is running *)

  end; (* CML_BASE *)
