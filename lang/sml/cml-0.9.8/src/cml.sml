(* cml.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure CML : INTERNAL_CML =
  struct

    structure CMLBase = CMLBase

  (* we must use the fully polymorphic versions of callcc, etc. *)
    open System.Unsafe.PolyCont

    open CMLBase

  (* some utility functions that should be inlined *)
    fun reverse ([], rl) = rl
      | reverse (x :: rest, rl) = reverse(rest, x :: rl)
    fun op o (f, g) = fn x => f(g x)
    fun map f l= let
	fun map' ([], l) = reverse(l, [])
	  | map' (x::r, l) = map'(r, (f x)::l)
	in
	  map' (l, [])
	end
    fun revmap f l= let
	fun map' ([], l) = l
	  | map' (x::r, l) = map'(r, (f x)::l)
	in
	  map' (reverse(l, []),  [])
	end
    fun a @ [] = a
      | a @ b = let
	  fun append ([], l) = reverse(l, b)
	    | append (x::r, l) = append(r, x::l)
	  in
	    append(a, [])
	  end
    fun op before (x, y) = x

    val earlier = System.Timer.earlier
    val add_time = System.Timer.add_time
    val zeroTime = TIME{sec=0, usec=0}

  (* create a new queue *)
    fun queueNew () = Q{front = ref [], rear = ref []}
  (* queue insert *)
    fun queueIns ((Q{rear, ...}), x) = (rear := x :: !rear)
  (* remove the head of the queue *)
    exception EmptyQ
    fun queueRem (Q{front = ref [], rear = ref []}) = raise EmptyQ
      | queueRem (Q{front = front as (ref []), rear = rear as (ref l)}) = let
	  val (x::r) = reverse(l, [])
	  in
	    front := r; rear := []; x
	  end
      | queueRem (Q{front = front as (ref(x::r)), ...}) = (front := r; x)


  (** Thread id creation **)
    val nextId = ref 0
    fun newId () = let val id = !nextId
	  in
	    nextId := id + 1;
	    TID{
		id = id,
		done_comm = ref true,
		death_cond = COND(ref(COND_unset[]))
	      }
	  end

  (* thread id marking *)
    fun markTid (TID{done_comm, ...}) = done_comm := true

  (* the current thread is represented using the "var" register *)
    val getCurThread : unit -> thread_id = System.Unsafe.getvar
    val setCurThread : thread_id -> unit = System.Unsafe.setvar
    val getTid = getCurThread
    val _ = setCurThread dummyTId

  (* enqueue a ready thread in the primary queue *)
    fun enqueue (p as (id, _)) = (markTid id; queueIns(rdyQ1, p))

  (* enqueue the current thread *)
    fun enqueueCurThread resume = (enqueue(getTid(), resume))

  (* remove a thread from the primary queue. *)
    fun dequeue1() = (case CMLBase.rdyQ1
	   of (Q{front = ref [], rear = ref []}) => CMLBase.dequeue2()
	    | (Q{front = front as (ref []), rear = rear as (ref l)}) => let
		 val (x::r) = reverse(l, [])
		 in
		   front := r; rear := []; x
		 end
	    | (Q{front = front as (ref(x::r)), ...}) => (front := r; x)
	  (* end case *))


  (* begin an atomic region *)
    fun atomicBegin () = (CMLBase.atomicState := Atomic)

  (* dispatch a thread while exiting an atomic region *)
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

    fun dispatch () = (atomicBegin(); atomicDispatch())

  (* end an atomic region *)
    fun atomicEnd () = (case !atomicState
	   of SignalPending => handlePendingSignal()
	    | _ => atomicState := NonAtomic
	  (* end case *))

  (* throw to a continuation while exiting an atomic region *)
    fun atomicThrow (k, x) = (
	  case !atomicState
	   of SignalPending => handlePendingSignal()
	    | _ => ()
	  (* end case *);
	  throw k x)


  (* channels *)
    datatype 'a chan = CHAN of {
	inq	: (thread_id * 'a cont) chanq,
	outq	: (thread_id * 'a * unit cont) chanq
      }
      withtype 'a chanq = (bool ref * 'a) queue_t

  (* events *)
    datatype abort_fn = NO_ABORT | ABORT of (unit -> unit)
    datatype 'a base_evt = BASE_EVT of {
	  pollfn : unit -> bool,
	  dofn : abort_fn -> 'a,
	  blockfn : (bool ref * abort_fn * (unit -> unit)) -> 'a,
	  abortfn : abort_fn
	}
    datatype 'a event
      = EVT of ('a base_evt list * bool)    (* the boolean is true if one of the *)
					    (* base events has an abort action *)
      | GUARD of (unit -> 'a event)

    fun mkBaseEvt arg = EVT([BASE_EVT arg], false)

    fun applyAbortFn NO_ABORT = ()
      | applyAbortFn (ABORT a) = a()

  (** Condition variables **)
    exception WriteTwice
    fun condVar () = COND(ref(COND_unset[]))
    fun writeVar (COND rc, x) = (
	  atomicBegin();
	  case (! rc)
	   of (COND_unset pl) => let
		fun f [] = ()
		  | f ((_, ref true, _) :: r) = (f r)
		  | f ((id, flg, kont) :: r) = (
		      enqueue (id, callcc (fn k1 => (
			callcc (fn k2 => throw k1 k2);
			throw kont x)));
		      flg := true;
		      f r)
		in
		  rc := (COND_set x);
		  f pl;
		  atomicEnd ()
		end
	    | _ => (atomicEnd(); raise WriteTwice)
	  (* end case *))
    fun addCondWaiter ([], w) = [w]
      | addCondWaiter ((_, ref true, _)::r, w) = addCondWaiter (r, w)
      | addCondWaiter (x::r, w) = x::(addCondWaiter(r, w))
    fun readVar (COND rc) = (
	  atomicBegin();
	  case !rc
	   of (COND_set x) => (atomicEnd(); x)
	    | (COND_unset pl) => callcc (fn k => (
		rc := COND_unset(addCondWaiter(pl, (getTid(), ref false, k)));
		atomicDispatch()))
	  (* end case *))
    fun readVarEvt (COND rc) = mkBaseEvt {
	    pollfn = fn () => (case !rc of (COND_set _) => true | _ => false),
	    dofn = fn abortfn => (
	      case !rc
	       of (COND_set x) => (atomicEnd(); applyAbortFn abortfn; x)
		| _ => error "[readVarEvt.dofn]"),
	    blockfn = fn (dirty, abortfn, next) => let
	      fun block k = (case !rc
		   of (COND_unset pl) => (
			rc := COND_unset(addCondWaiter(pl, (getTid(), dirty, k)));
			next(); error "[readVarEvt]")
		    | _ => error "[readVarEvt.blockfn]")
	      in
		case abortfn
		 of NO_ABORT => (callcc block)
		  | (ABORT f) => ((callcc block) before (f ()))
	      end,
	    abortfn = NO_ABORT
	  }


  (** Channel operations **)
    fun insert (q : 'a chanq, flg, item) = queueIns(q, (flg, item))
    fun remove (q : 'a chanq) = let
	  val (flg, item) = queueRem q
	  in
	    flg := true; item
	  end

  (* Clean a channel of satisfied transactions.	 We do this incrementally to
   * give an amortized constant cost.  Basically we guarantee that the front
   * of the queue will be unsatisfied.	Return true if the resulting queue
   * is non-empty.
   *)
    local
      fun clean' [] = []
	| clean' ((ref true, _) :: r) = clean' r
	| clean' l = l
    in
    fun clean ((Q{front, rear}) : 'a chanq) = (case (front, rear)
	 of (ref [], ref []) => false
	  | (ref [], ref r) => (case clean'(reverse(r, []))
	     of [] => (rear := []; false)
	      | l => (front := l; rear := []; true))
	  | (ref f, ref r) => (case (clean' f)
	     of [] => (case clean'(reverse(r, []))
		 of [] => (front := []; rear := []; false)
		  | l => (front := l; rear := []; true))
	      | l => (front := l; true)))
    fun cleanAndRemove ((Q{front, rear}) : 'a chanq) = (case (front, rear)
	 of (ref [], ref []) => NONE
	  | (ref [], ref r) => (case clean'(reverse(r, []))
	     of [] => (rear := []; NONE)
	      | ((flg, item)::rest) => (
		  front := rest; rear := []; flg := true; SOME item))
	  | (ref f, ref r) => (case (clean' f)
	     of [] => (case clean'(reverse(r, []))
		 of [] => (front := []; rear := []; NONE)
		  | ((flg, item)::rest) => (
		      front := rest; rear := []; flg := true; SOME item))
	      | ((flg, item)::rest) => (front := rest; flg := true; SOME item)))
    end (* local *)

  (* remove any waiting threads from a channel's queues *)
    fun resetChan (CHAN{inq=Q{front=f1, rear=r1}, outq=Q{front=f2, rear=r2}}) = (
	  f1 := []; r1 := []; f2 := []; r2 := [])

  (* channel : unit -> '1a chan *)
    fun channel () = CHAN{inq = queueNew(), outq = queueNew()}

  (* sameChannel : ('a chan * 'a chan) -> bool *)
    fun sameChannel (CHAN{inq=Q{front=f1, ...}, ...}, CHAN{inq=Q{front=f2, ...}, ...}) =
	  (f1 = f2)

  (* send : ('a chan * 'a) -> unit *)
    fun send (CHAN{inq, outq}, msg) = callcc (fn send_k => (
	  atomicBegin();
	  case (cleanAndRemove inq)
	   of SOME(rid, rkont) => (
		enqueueCurThread send_k;
		setCurThread rid;
		atomicThrow (rkont, msg))
	    | NONE => (
		insert(outq, ref false, (getTid(), msg, send_k));
		atomicDispatch())
	  (* end case *)))
    fun sendc ch msg = send (ch, msg)

  (* accept : 'a chan -> 'a *)
    fun accept (CHAN{inq, outq}) = callcc (fn accept_k => (
	  atomicBegin();
	  case (cleanAndRemove outq)
	   of SOME(sid, msg, skont) => (
		enqueue (sid, skont);
		atomicThrow (accept_k, msg))
	    | NONE => (
		insert(inq, ref false, (getTid(), accept_k));
		atomicDispatch())
	  (* end case *)))

  (* transmit : ('a chan * 'a) -> unit event *)
    fun transmit (CHAN{inq, outq}, msg) = let
	  fun pollFn () = (clean inq)
	  fun doFn abortfn = let
		val (rid, rkont) = remove inq
		fun doit k = (
		      enqueueCurThread k;
		      setCurThread rid;
		      atomicThrow(rkont, msg))
		in
		  case abortfn
		   of NO_ABORT => callcc doit
		    | (ABORT f) => (callcc doit; f())
		end
	  fun blockFn (flg, abortfn, next) = let
		fun block k = (
		      clean outq;
		      insert(outq, flg, (getTid(), msg, k));
		      next(); error "[transmit]")
		in
		  case abortfn
		   of NO_ABORT => (callcc block)
		    | (ABORT f) => (callcc block; f())
		end
	  in
	    mkBaseEvt {
		pollfn = pollFn, dofn = doFn, blockfn = blockFn, abortfn = NO_ABORT
	      }
	  end
    fun transmitc ch msg = transmit (ch, msg)

  (* receive : 'a chan -> 'a event *)
    fun receive (CHAN{inq, outq}) = let
	  fun pollFn () = (clean outq)
	  fun doFn abortfn = let
		val (sid, msg, skont) = remove outq
		in
		  enqueue (sid, skont);
		  atomicEnd ();
		  applyAbortFn abortfn;
		  msg
		end
	  fun blockFn (flg, abortfn, next) = let
		fun block k = (
		      clean inq;
		      insert(inq, flg, (getTid(), k));
		      next(); error "[receive]")
		in
		  case abortfn
		   of NO_ABORT => (callcc block)
		    | (ABORT f) => ((callcc block) before (f ()))
		end
	  in
	    mkBaseEvt {
		pollfn = pollFn, dofn = doFn, blockfn = blockFn, abortfn = NO_ABORT
	      }
	  end

  (* A channel to pass errors to an error monitor *)
    val errCh : (thread_id * exn) chan = channel()


  (** Thread operations **)
    fun notify () = let
	  val (TID{id, death_cond, ...}) = getTid()
	  in
	    writeVar (death_cond, ())
	  end
    fun spawnc f x = let
	  val _ = atomicBegin()
	  val id = newId()
	  in
	    callcc (fn parent_k => (
	      enqueueCurThread parent_k;
	      setCurThread id;
	      atomicEnd();
	      (f x; notify())
		handle ex => (notify (); send (errCh, ((getTid()), ex)));
	      dispatch ()));
	    id
	  end
    fun spawn f = spawnc f ()

    fun fastSpawn f = let
	  val _ = atomicBegin()
	  val id = newId()
	  in
	    callcc (fn k => (
	      callcc (fn k' => (enqueue (id, k'); atomicThrow (k, ())));
	      (f (); notify())
		handle ex => (notify (); send (errCh, (getTid(), ex)));
	      dispatch ()))
	  end
    val spawnList = app fastSpawn

  (* terminate the current thread *)
    fun exit () = (notify(); dispatch())

    fun threadWait (TID{death_cond, ...}) = readVarEvt death_cond

  (* test thread_ids for equality *)
    fun sameThread (TID{id=p1, ...}, TID{id=p2, ...}) = (p1 = p2)
  (* test the order of thread ids *)
    fun tidLessThan (TID{id=p1, ...}, TID{id=p2, ...}) = (p1 < p2)
  (* return a string representation of a thread id *)
    fun tidToString (TID{id, ...}) = implode ["[", makestring id, "]"]

  (* yield control to the next thread *)
    fun yield () = callcc (fn k => (
	  atomicBegin(); enqueueCurThread k; atomicDispatch()))


  (** Event operations **)
    fun dummyFn _ = error "[dummyFn]"

  (* always : 'a -> 'a event *)
    fun always x = mkBaseEvt {
	    pollfn = (fn () => true),
	    dofn = (fn abort => (applyAbortFn abort; x)),
	    blockfn = dummyFn,
	    abortfn = NO_ABORT
	  }
    val ALWAYS = always ()

  (* 'a event list -> 'a event *)
    fun choose l = let
	  fun f ([], el, [], flg) = EVT(el, flg)
	  (* note that the guard list gl is in reverse order *)
	    | f ([], el, gl, flg) =
		GUARD(fn () =>
		  choose (EVT(el, flg) :: (revmap (fn g => (g ())) gl)))
	    | f (EVT(el', false) :: r, el, gl, hasAbort) =
		f (r, el' @ el, gl, hasAbort)
	    | f (EVT(el', true) :: r, el, gl, _) = f (r, el' @ el, gl, true)
	    | f ((GUARD g)::r, el, gl, hasAbort) = f (r, el, g::gl, hasAbort)
	  in
	    f (l, [], [], false)
	  end

  (* guard : (unit -> 'a event) -> 'a event *)
    val guard = GUARD

  (* wrap : ('a event * ('a -> 'b)) -> 'b event *)
    fun wrap (GUARD g, f) = GUARD(fn () => wrap(g (), f))
      | wrap (EVT(el, flg), f) = let
	  fun wrapEvts ([], l) = l
	    | wrapEvts ((BASE_EVT{pollfn, dofn, blockfn, abortfn})::r, l) = let
		val bev = BASE_EVT{
			pollfn = pollfn,
			dofn = (f o dofn),
			blockfn = (f o blockfn),
			abortfn = abortfn
		      }
		in
		  wrapEvts(r, bev::l)
		end
	  in
	    EVT(wrapEvts (el, []), flg)
	  end

  (* wrapHandler : ('a event * (exn -> 'a)) -> 'a event *)
    fun wrapHandler (GUARD g, h) =GUARD(fn () => wrapHandler(g (), h))
      | wrapHandler (EVT(el, flg), h) = let
	  fun wh f x = (f x) handle e => (h e)
	  fun wrapEvts ([], l) = l
	    | wrapEvts ((BASE_EVT{pollfn, dofn, blockfn, abortfn})::r, l) = let
		val bev = BASE_EVT{
			pollfn = pollfn,
			dofn = (wh dofn),
			blockfn = (wh blockfn),
			abortfn = abortfn
		      }
		in
		  wrapEvts(r, bev::l)
		end
	  in
	    EVT(wrapEvts (el, []), flg)
	  end

  (* wrapAbort : (a event * (unit -> unit)) -> 'a event *)
    fun wrapAbort (GUARD g, abort) = GUARD(fn () => wrapAbort (g (), abort))
      | wrapAbort (EVT(el, flg), abort) = let
	  fun addAbortFn (BASE_EVT{pollfn, dofn, blockfn, abortfn}, abort) = BASE_EVT{
		  pollfn = pollfn,
		  dofn = dofn,
		  blockfn = blockfn,
		  abortfn = (case abortfn
		     of NO_ABORT => ABORT abort
		      | (ABORT a) => ABORT(fn () => (fastSpawn abort; a())))
		}
	  in
	    case el
	     of [] => EVT([], false)
	      | [bev] => EVT([addAbortFn (bev, abort)], true)
	      | (leader :: followers) => let
		  val n = length followers
		  in
		    GUARD (fn () => let
		      val ackCh = channel()
		      fun followerAbort () = send(ackCh, ())
		      fun leaderAbort 0 = abort()
		        | leaderAbort i = (accept ackCh; leaderAbort(i-1))
		      in
			EVT(
			  addAbortFn(leader, fn () => (leaderAbort n)) ::
			    (map (fn b => addAbortFn(b, followerAbort)) followers),
			  true)
		      end)
		  end
	  end

  (** Sync and poll **)
    local
    (* Generate index numbers for "non-deterministic" selection.  We use a
     * round-robin style policy. *)
      val cnt = ref 0
      fun random 1 = 0
	| random i = let val j = !cnt
	    in
	      cnt := Bits.andb(j+1, 0x7fff); (j rem i)
	    end
      fun selectDoFn (el, n) = let
	    fun sel (f::_, 0) = f NO_ABORT
	      | sel (_::r, n) = sel (r, n-1)
	    in
	      sel (el, random n)
	    end

    fun syncOnEvts ([], _) = exit()
      | syncOnEvts ([BASE_EVT{pollfn, dofn, blockfn, ...}], _) = (
	    atomicBegin();
	    if (pollfn ())
	      then dofn NO_ABORT
	      else blockfn (ref false, NO_ABORT, atomicDispatch))
      | syncOnEvts (bevs, false) = let
	  fun ext ([], blockFns) = capture (fn k => let
		val escape = escape k
		val dirtyFlg = ref false
		fun log [] = atomicDispatch ()
		  | log (blockfn :: r) = escape (
		      blockfn (dirtyFlg, NO_ABORT, fn () => log r))
		in
		  log blockFns; error "[log]"
		end)
	    | ext (BASE_EVT{pollfn, dofn, blockfn, ...} :: r, blockFns) =
		if (pollfn ())
		  then extRdy (r, [dofn], 1)
		  else ext (r, blockfn::blockFns)
	  and extRdy ([], doFns, n) = selectDoFn (doFns, n)
	    | extRdy (BASE_EVT{pollfn, dofn, ...} :: r, doFns, n) =
		if (pollfn ())
		  then extRdy (r, dofn::doFns, n+1)
		  else extRdy (r, doFns, n)
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end
      | syncOnEvts (bevs, true) = let
	  datatype 'a bevt_status
	    = BLK of 'a block_fn_t
	    | BLK_ABORT of ('a block_fn_t * (unit -> unit))
	    | RDY of (abort_fn -> 'a)
	    | RDY_ABORT of ((abort_fn -> 'a) * (unit -> unit))
	  withtype 'a block_fn_t = (bool ref * abort_fn * (unit -> unit)) -> 'a
	  fun ext ([], sts) = let
		fun projAbortFns [] = []
		  | projAbortFns (BLK_ABORT(_, abort)::r) = abort :: (projAbortFns r)
		  | projAbortFns (_::r) = projAbortFns r
		val abortFns = projAbortFns sts
		val allAborts = ABORT(fn () => spawnList abortFns)
		fun mkAbortFn i = let
		      fun abort ([], _) = ()
			| abort (a::r, j) = (
			    if (i <> j) then (fastSpawn a; ()) else ();
			    abort (r, j+1))
		      in
			ABORT(fn () => abort(abortFns, 0))
		      end
		in
		  capture (fn k => let
		    val escape = escape k
		    val dirtyFlg = ref false
		    fun log ([], _) = atomicDispatch ()
		      | log ((BLK bfn) :: r, i) = escape (
			  bfn (dirtyFlg, allAborts,
			    fn () => (log(r, i); error "[log]")))
		      | log ((BLK_ABORT(bfn, _)) :: r, i) = escape (
			  bfn (dirtyFlg, mkAbortFn i,
			    fn () => (log(r, i+1); error "[log]")))
		      | log _ = error "[log]"
		    in
		      log (sts, 0)
		    end)
		end
	    | ext (BASE_EVT{pollfn, dofn, blockfn, abortfn} :: r, sts) = (
		case (pollfn(), abortfn)
		 of (false, NO_ABORT) => ext (r, (BLK blockfn)::sts)
		  | (false, ABORT a) => ext (r, (BLK_ABORT(blockfn, a))::sts)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, 1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, 1)
		(* end case *))
	  and extRdy ([], sts, nRdy) = let
		fun selAndAbortRest ([], _, _) = error "[selAndAbortRest]"
		  | selAndAbortRest ((BLK _)::r, i, abortFns) =
		      selAndAbortRest (r, i, abortFns)
		  | selAndAbortRest ((BLK_ABORT(_, abort))::r, i, abortFns) =
		      selAndAbortRest (r, i, abort::abortFns)
		  | selAndAbortRest ((RDY doFn)::r, 0, abortFns) =
		      abortRest (r, abortFns, doFn)
		  | selAndAbortRest ((RDY _)::r, i, abortFns) =
		      selAndAbortRest (r, i-1, abortFns)
		  | selAndAbortRest ((RDY_ABORT(doFn, _))::r, 0, abortFns) =
		      abortRest (r, abortFns, doFn)
		  | selAndAbortRest ((RDY_ABORT(_, abort))::r, i, abortFns) =
		      selAndAbortRest (r, i-1, abort::abortFns)
		and abortRest ([], abortFns, doFn) =
		      doFn (ABORT(fn () => spawnList abortFns))
		  | abortRest ((BLK_ABORT(_, abort))::r, abortFns, doFn) =
		      abortRest (r, abort::abortFns, doFn)
		  | abortRest ((RDY_ABORT(_, abort))::r, abortFns, doFn) =
		      abortRest (r, abort::abortFns, doFn)
		  | abortRest (_::r, abortFns, doFn) =
		      abortRest (r, abortFns, doFn)
		in
		  selAndAbortRest (sts, random nRdy, [])
		end
	    | extRdy (BASE_EVT{pollfn, dofn, blockfn, abortfn} :: r, sts, nRdy) = (
		case (pollfn(), abortfn)
		 of (false, NO_ABORT) => extRdy (r, sts, nRdy)
		  | (false, ABORT a) => extRdy (r, (BLK_ABORT(blockfn, a))::sts, nRdy)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, nRdy+1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, nRdy+1)
		(* end case *))
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end

    fun pollEvts ([], _) = NONE
      | pollEvts ([BASE_EVT{pollfn, dofn, abortfn, ...}], _) = (
	    atomicBegin();
	    if (pollfn ())
	      then SOME(dofn abortfn)
	      else (
		case abortfn of (ABORT a) => (fastSpawn a; ()) | _ => ();
		atomicEnd(); NONE))
      | pollEvts (bevs, false) = let
	  fun ext [] = (atomicEnd(); NONE)
	    | ext (BASE_EVT{pollfn, dofn, ...} :: r) =
		if (pollfn ()) then extRdy(r, [dofn], 1) else ext r
	  and extRdy ([], doFns, n) = SOME(selectDoFn (doFns, n))
	    | extRdy (BASE_EVT{pollfn, dofn, ...} :: r, doFns, n) =
		if (pollfn ())
		  then extRdy (r, dofn::doFns, n+1)
		  else extRdy (r, doFns, n)
	  in
	    atomicBegin();
	    ext bevs
	  end
      | pollEvts (bevs, true) = let
	  datatype 'a bevt_status
	    = BLK_ABORT of (unit -> unit)
	    | RDY of (abort_fn -> 'a)
	    | RDY_ABORT of ((abort_fn -> 'a) * (unit -> unit))
	  fun doAndAbort (sts, i) = let
		fun f ([], abortFns, _, NONE) = (spawnList abortFns; NONE)
		  | f ([], abortFns, _, SOME doFn) =
			SOME (doFn (ABORT(fn () => spawnList abortFns)))
		  | f ((BLK_ABORT abort) :: r, abortFns, i, doFn) =
			f (r, abort::abortFns, i, doFn)
		  | f ((RDY doFn) :: r, abortFns, 0, _) =
			f (r, abortFns, i, SOME doFn)
		  | f ((RDY _) :: r, abortFns, i, doFn) =
			f (r, abortFns, i-1, doFn)
		  | f ((RDY_ABORT(doFn, _)) :: r, abortFns, 0, _) =
			f (r, abortFns, i, SOME doFn)
		  | f ((RDY_ABORT(_, abort)) :: r, abortFns, i, doFn) =
			f (r, abort::abortFns, i, doFn)
		in
		  f (sts, [], i, NONE)
		end
	  fun ext ([], sts) = doAndAbort (sts, ~1)
	    | ext (BASE_EVT{pollfn, dofn, abortfn, ...} :: r, sts) = (
		case (pollfn (), abortfn)
		 of (false, NO_ABORT) => ext (r, sts)
		  | (false, ABORT a) => ext (r, (BLK_ABORT a)::sts)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, 1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, 1)
		(* end case *))
	  and extRdy ([], sts, n) = doAndAbort (sts, random n)
	    | extRdy (BASE_EVT{pollfn, dofn, abortfn, ...} :: r, sts, n) = (
		case (pollfn (), abortfn)
		 of (false, NO_ABORT) => extRdy (r, sts, n)
		  | (false, ABORT a) => extRdy (r, (BLK_ABORT a)::sts, n)
		  | (true, NO_ABORT) => extRdy (r, (RDY dofn)::sts, n+1)
		  | (true, ABORT a) => extRdy (r, (RDY_ABORT(dofn, a))::sts, n+1)
		(* end case *))
	  in
	    atomicBegin();
	    ext (bevs, [])
	  end

    fun evalGuard (GUARD g) = evalGuard (g ())
      | evalGuard (EVT arg) = arg
    in
    fun sync evt = syncOnEvts (evalGuard evt)
    val select = sync o choose
    fun poll evt = pollEvts (evalGuard evt)
    end (* local *)

  (* waitUntil : time -> unit event *)
    fun waitUntil (t as TIME{sec, usec}) = let
	  val curTime = currentTime()
	  in
	    if earlier(curTime, t)
	      then let
	        fun pollFn () = false
		fun blockFn (flg, abort, next) = let
		      fun block k = (
			    insTimeWait(t, getTid(), k, flg);
			    next())
		      in
			case abort
			 of NO_ABORT => (callcc block)
			  | (ABORT f) => (callcc block; f ())
		      end
		in
		  mkBaseEvt {
		      pollfn = pollFn, dofn = dummyFn, blockfn = blockFn,
		      abortfn = NO_ABORT
		    }
		end
	      else ALWAYS
	  end

  (* timeout : time -> unit event *)
    fun timeout (t as TIME{sec, usec}) = if earlier(t, zeroTime)
	  then ALWAYS
	  else let
	    fun pollFn () = false
	    fun blockFn (flg, abort, next) = let
		  fun block k = (
			insTimeWait(add_time (currentTime(), t), getTid(), k, flg);
			next())
		  in
		    case abort
		     of NO_ABORT => (callcc block)
		      | (ABORT f) => (callcc block; f ())
		  end
	    in
	      mkBaseEvt {
		  pollfn = pollFn, dofn = dummyFn, blockfn = blockFn,
		  abortfn = NO_ABORT
		}
	    end


  (* low-level I/O support (not for general use) *)
    exception InvalidFileDesc of int
    local
      fun blockFn (fd, io_op) (flg, abort, next) = (
	    callcc (fn okay_k => (
	      callcc (fn err_k => (
		insIOWait {
		    fd=fd, io_op=io_op, dirty=flg, kont=okay_k,
		    err_kont=err_k, id=getTid()
		  };
		next()));
	      applyAbortFn abort;
	      raise (InvalidFileDesc fd)));
	    applyAbortFn abort)
      fun mkIOEvt (pollFn, ioOp) fd = mkBaseEvt {
	      pollfn = pollFn, dofn = applyAbortFn, blockfn = blockFn(fd, ioOp),
	      abortfn = NO_ABORT
	    }
    in
    fun syncOnInput fd = let
	  fun pollFn () = (
		case pollFDs([fd], [], [], false) of ([], _, _) => false | _ => true)
		  handle _ => false
	  in
	    mkIOEvt (pollFn, IO_RD) fd
	  end
    fun syncOnOutput fd = let
	  fun pollFn () = (
		case pollFDs([], [fd], [], false) of (_, [], _) => false | _ => true)
		  handle _ => false
	  in
	    mkIOEvt (pollFn, IO_WR) fd
	  end
    fun syncOnExcept fd = let
	  fun pollFn () = (
		case pollFDs([], [], [fd], false) of (_, _, []) => false | _ => true)
		  handle _ => false
	  in
	    mkIOEvt (pollFn, IO_EX) fd
	  end
    end (* local fun blockFn ... *)


  (** Initialization **)

  (* initialize the internal queues, I/O waiting lists and counters *)
    fun initCML () = (
	  nextId := 0;
	  setCurThread (newId());
	  initCMLBase ())

  end; (* CML *)

