(* cml-base.sml
 *)

structure CMLBase : CML_BASE =
  struct

    open CMLVersion (* for version and versionName *)

  (* some utility functions that should be inlined *)
    fun reverse ([], rl) = rl
      | reverse (x :: rest, rl) = reverse(rest, x :: rl)

  (* queues *)
    datatype 'a queue_t = Q of {front : 'a list ref, rear : 'a list ref}

  (* create a new queue *)
    fun queueNew () = Q{front = ref [], rear = ref []}

  (* queue insert *)
    fun queueInsc (Q{rear, ...}) = fn x => (rear := x :: !rear)

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

    val dummyTId = TID{
	    id = ~1, done_comm = ref false, death_cond = COND(ref(COND_set()))
	  }

  (* report an internal error on std_err *)
    fun reportError msg = let
	  val s = if (ord(msg) = ord("\n"))
	  	then implode["\nCML: ", substring(msg, 1, (size msg)-1), "\n"]
		else implode["CML: ", msg, "\n"]
	  in
	    System.Signals.maskSignals true;
	    IO.output(IO.std_err, s);
	    System.Signals.maskSignals false
	  end

    exception InternalError
    fun error s = (reportError("\nINTERNAL ERROR: "^s); raise InternalError)


  (* the termination function *)
    val shutdown = ref (fn () => ())


  (* timers *)
    structure T : sig
        datatype time = TIME of {sec : int, usec : int}
	val earlier : time * time -> bool
	val add_time : time * time -> time
	val zeroTime : time
        val currentTime : unit -> time
	val timerOff : unit -> unit
	val timerOn : time option -> unit
	val restartTimer : unit -> unit
      end = struct
        open System.Timer System.Unsafe.CInterface
	val zeroTime = TIME{sec=0, usec=0}
	val tod : unit -> (int * int) =
	      wrap_sysfn "timeofday" (c_function "timeofday")
	fun currentTime () = let
	      val (s, u) = tod()
	      in
		TIME{sec = s, usec = u}
	      end
        val saveTime = ref (NONE : time option)
	fun timerOff () = setitimer (0, zeroTime, zeroTime)
	fun timerOn t = (
	      saveTime := t;
	      case t of (SOME tq) => setitimer (0, tq, tq) | _ => ())
	fun restartTimer () = 
	      case !saveTime of (SOME tq) => setitimer (0, tq, tq) | _ => ()
      end
    open T


  (* thread id marking *)
    fun markTid (TID{done_comm, ...}) = done_comm := true
    fun unmarkTid (TID{done_comm, ...}) = done_comm := false
    fun isMarked (TID{done_comm, ...}) = !done_comm

  (* the current thread is represented using the "var" register *)
    val getCurThread : unit -> thread_id = System.Unsafe.getvar
    val setCurThread : thread_id -> unit = System.Unsafe.setvar

  (* The thread ready queues:
   * rdyQ1 is the primary queue and rdyQ2 is the secondary queue.
   *)
    val rdyQ1 : (thread_id * unit cont) queue_t = queueNew()
    val rdyQ2 : (thread_id * unit cont) queue_t = queueNew()

  (* enqueue a ready thread *)
    val enqueue1 = queueInsc rdyQ1
    val enqueue2 = queueInsc rdyQ2
    fun enqueue (p as (id, _)) = (markTid id; enqueue1 p)

  (* enqueue the current thread *)
    fun enqueueCurThread resume = (enqueue(getCurThread(), resume))

  (* promote a thread from the secondary queue to the primary queue *)
    fun promote () = (case rdyQ2
	   of Q{front=ref [], rear=ref []} => ()
	    | Q{front=front as (ref []), rear} => let
		val (x::r) = reverse (!rear, [])
		in
		  front := r; rear := []; enqueue1 x
		end
	    | Q{front=front as ref (x::r), rear} => (front := r; enqueue1 x)
	  (* end case *))

  (* preempt the current thread (with continuation k). *)
    fun preempt k = let
	  val curTid = getCurThread()
	  val curP = (curTid, k)
	  in
	    if (isMarked curTid)
	      then (
		unmarkTid curTid;
		promote ();
		enqueue1 curP)
	      else enqueue2 curP
	  end


  (** I/O wait queues **)
    fun pollFDs (rdfds, wrfds, exfds, blocking) = let
	  val t = if blocking then NONE else (SOME zeroTime)
	  val (rd, wr, ex) = System.Unsafe.SysIO.select(rdfds, wrfds, exfds, t)
	  in
	    (rd, wr, ex)
	  end
    (* The list of I/O wait events *)
      datatype io_operation_t = IO_RD | IO_WR | IO_EX
      type io_item = {
	  fd	   : int,		(* the file descriptor *)
	  io_op	   : io_operation_t,	(* the operation being waited for *)
	  kont	   : unit cont,		(* the synchronization continuation and *)
	  id	   : thread_id,		(* the id of the waiting thread *)
	  err_kont : unit cont,		(* the error continuation of the thread *)
	  dirty	   : bool ref		(* the dirty bit *)
	}
      val ioWaitList = ref ([] : io_item list)

    (* project the different kinds of I/O operations in the I/O wait list. *)
      fun projIO () = let
	    fun f ([] : io_item list, rd, wr, ex) = (rd, wr, ex)
	      | f ({dirty = ref true, ...}::r, rd, wr, ex) = f(r, rd, wr, ex)
	      | f ({io_op = IO_RD, fd, ...}::r, rd, wr, ex) = f(r, fd::rd, wr, ex)
	      | f ({io_op = IO_WR, fd, ...}::r, rd, wr, ex) = f(r, rd, fd::wr, ex)
	      | f ({io_op = IO_EX, fd, ...}::r, rd, wr, ex) = f(r, rd, wr, fd::ex)
	    in
	      f(!ioWaitList, [], [], [])
	    end

    (* check for available I/O operations *)
      fun checkIO shouldBlock = (case projIO()
	     of ([], [], []) => ()
	      | (rd, wr, ex) => (case pollFDs(rd, wr, ex, shouldBlock)
		   of ([], [], []) => ()
		    | (rd, wr, ex) => let
			fun f ([], l) = l
			  | f (({dirty = ref true, ...} : io_item)::r, l) = f (r, l)
			  | f ((x as {io_op, fd, kont, id, dirty, ...})::r, l) = let
			      fun look [] = false
				| look (x::r) = if (x = fd)
				    then (
				      enqueue(id, kont);
				      dirty := true;
				      true)
				    else (look r)
			      val fdList = (case io_op
				     of IO_RD => rd
				      | IO_WR => wr
				      | IO_EX => ex)
			      in
				if (look fdList) then f(r, l) else f(r, x::l)
			      end
			in
			  ioWaitList := f(!ioWaitList, [])
			end
		  (* end case *))
	    (* end case *))
	    handle (System.Unsafe.CInterface.SystemCall _) => let
	      open System.Unsafe.SysIO
	      fun testDesc fd = (ftype(DESC fd); false) handle _ => true
	      fun findBadDescs ([], l) = l
		| findBadDescs ((x as {fd, dirty, err_kont, id, ...} : io_item)::r, l) =
		    if (testDesc fd)
		      then (
			enqueue(id, err_kont);
			dirty := true;
			findBadDescs (r, l))
		      else findBadDescs (r, x::l)
	      in
		ioWaitList := findBadDescs(!ioWaitList, []);
		checkIO shouldBlock
	      end

    (* insert an I/O operation into the I/O waiting list *)
      fun insIOWait info = ioWaitList := info :: !ioWaitList

    (* return true if there is at least one clean I/O wait event on the list *)
      fun waitingForIO () = let
	    fun f (l as (({dirty = ref true, ...}::r)) : io_item list) = (f r)
	      | f l = l
	    in
	      case f(!ioWaitList)
	       of [] => (ioWaitList := []; false)
		| l => (ioWaitList := l; true)
	    end


  (** Timer waiting queues **)
    datatype time_wait_t = TIMEWAIT of {
	wait_time : time,
	id : thread_id,
	kont : unit cont,
	dirty : bool ref
      }
    val timeWaitList = ref ([] : time_wait_t list)

  (* insert a timeout event *)
    fun insTimeWait (tim, id, k, flg) = let
	  val item = TIMEWAIT{wait_time=tim, id=id, kont=k, dirty=flg}
	  fun scan [] = [item]
	    | scan ((t as TIMEWAIT{dirty = ref true, ...})::r) = scan r
	    | scan (l as ((t as TIMEWAIT{wait_time, ...})::r)) =
		if (earlier (tim, wait_time))
		  then (item::l)
		  else (t::(scan r))
	  in
	    timeWaitList := scan(!timeWaitList)
	  end

  (* schedule any threads waiting for times earlier than the current time. *)
    fun remTimeWait () = let
	  val tim = currentTime()
	  fun scan [] = []
	    | scan (l as ((t as TIMEWAIT{dirty = ref true, ...})::r)) = scan r
	    | scan (l as ((t as TIMEWAIT{wait_time, id, kont, dirty})::r)) =
		if earlier(tim, wait_time)
		  then l
		  else (enqueue(id, kont); dirty := true; scan r)
	  in
	    timeWaitList := scan(!timeWaitList)
	  end

  (* return true if there is at least one clean timeout event on the list *)
    fun waitingForTimeout () = let
	  fun f (TIMEWAIT{dirty = ref true, ...}::r) = (f r)
	    | f l = l
	  in
	    case (f (!timeWaitList))
	     of [] => (timeWaitList := []; false)
	      | l => (timeWaitList := l; true)
	  end
    structure Sig = System.Signals


  (*  test for blocked threads that could conceivably become unblocked *)
    fun checkWaitingThreads () = (
	 case (!ioWaitList) of [] => () | _=> (checkIO false);
	 case (!timeWaitList) of [] => () | _=> remTimeWait ())

  (* global flag for implementing atomic operations *)
    datatype atomic_state = NonAtomic | Atomic | SignalPending
    val atomicState = ref NonAtomic

  (* remove a thread from the primary queue.  *)
    fun dequeue1 () = (case rdyQ1
	   of (Q{front = ref [], rear = ref []}) => dequeue2()
	    | (Q{front = front as (ref []), rear = rear as (ref l)}) => let
		val (x::r) = reverse(l, [])
		in
		  front := r; rear := []; x
		end
	    | (Q{front = front as (ref(x::r)), ...}) => (front := r; x)
	  (* end case *))
  (* remove a thread from the secondary queue (assuming that the
   * primary queue is empty.
   *)
    and dequeue2 () = let
	(* wait for I/O or delay when there are no ready threads. *)
	  fun waitForSomething () = (case (waitingForIO(), waitingForTimeout())
	       of (false, false) => (!shutdown)()
		| (_, false) => (timerOff(); checkIO true; restartTimer())
		| _ => (System.Signals.pause(); checkWaitingThreads()))
	  fun dequeue2 () = (case rdyQ2
	       of (Q{front = ref [], rear = ref []}) => (waitForSomething(); dequeue1())
		| (Q{front = front as (ref []), rear = rear as (ref l)}) => let
		    val (x::r) = reverse(l, [])
		    in
		      front := r; rear := []; x
		    end
		| (Q{front = front as (ref(x::r)), ...}) => (front := r; x))
	  in
	    dequeue2()
	  end (* dequeue2 *)

    fun atomicDispatch () = let
	  val _ = (case !atomicState
		 of SignalPending => checkWaitingThreads()
		  | _ => ()
		(* end case *))
	  val (id, kont) = dequeue1()
	  in
	    setCurThread id;
	    atomicState := NonAtomic;
	    throw kont ()
	  end

  (* Complete the exit from an atomic region when there is a pending signal. *)
    fun handlePendingSignal () =
	  callcc (fn k => (preempt k; atomicDispatch()))

  (* initialize the atomic region support *)
    fun initAtomic () = let
	  val checkIOKont = callcc (fn k1 => (
		callcc (fn k2 => (throw k1 k2));
	      (* NOTE: this continuation always starts in an atomic region
	       * with atomicState = SignalPending.
	       *)
		atomicDispatch()))
	  fun alrm_handler (_, k) = (case !atomicState
		 of NonAtomic => (
		      preempt k;
		    (* We set the atomicState to SignalPending to force a
		     * check for I/O and timers in checkIOKont.
		     *)
		      atomicState := SignalPending;
		      checkIOKont)
		  | _ => (atomicState := SignalPending;  k)
		(* end case *))
	  in
	    Sig.setHandler (Sig.SIGALRM, SOME alrm_handler);
	    atomicState := NonAtomic
	  end

  (* return the # of threads created *)
    fun load () = let
	  val _ = (atomicState := Atomic)
	  fun count (Q{front, rear}) = List.length(!front) + List.length(!rear)
	  val res = (count rdyQ1 + count rdyQ2)
	  in
	    case !atomicState
	     of SignalPending => handlePendingSignal()
	      | _ => atomicState := NonAtomic
	    (* end case *);
	    res
	  end

    fun initCMLBase () = let
	  fun emptyQ (Q{front, rear}) = (front := []; rear := [])
	  in
	    emptyQ rdyQ1; emptyQ rdyQ2;
	    ioWaitList := [];
	    timeWaitList := [];
	    initAtomic()
	  end

    val running = ref false

  (* mark the beginning of CML execution and start the timer *)
    fun go tq = (running := true; timerOn tq)

  (* turn the timer off, and mark the end of CML execution *)
    fun stop () = (timerOff(); running := false)

  (* returns true if CML is running *)
    fun isRunning () = !running

  end; (* CMLBase *)

