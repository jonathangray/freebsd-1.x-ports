(* trace-cml.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * This module provides rudimentary debugging support in the form of mechanisms
 * to control debugging output, and to monitor thread termination.  This
 * version of this module is adapted from Cliff Krumvieda's utility for tracing
 * CML programs.  It provides three facilities: trace modules, for controlling
 * debugging output; thread watching, for detecting thread termination; and
 * a mechanism for reporting uncaught exceptions on a per thread basis.
 *)

functor TraceCML (
    structure CML : INTERNAL_CML
          and RunCML : RUN_CML
	  and CIO : CONCUR_IO
    sharing CML = RunCML.CML = CIO.CML
    sharing type CML.thread_id = CML.CMLBase.thread_id
  ) : TRACE_CML = struct

    open CML (* need to open INTERNAL_CML version before rebinding CML *)

    structure CIO : CONCUR_IO = CIO
    structure CML : CONCUR_ML = CML

  (* where to direct trace output to *)
    datatype trace_to
      = TraceToOut
      | TraceToErr
      | TraceToNull
      | TraceToFile of string
      | TraceToStream of CIO.outstream

    exception NoSuchModule

  (** Trace Modules **)
    datatype trace_module = TM of {
	full_name : string,
	label : string,
	tracing : bool ref,
	children : trace_module list ref
      }

    val traceRoot = TM{
	    full_name = "/",
	    label = "/",
	    tracing = ref false,
	    children = ref []
	  }

    fun fullName "" = "/"
      | fullName s = if (ordof(s, size s - 1) = (* "/" *)47) then s else s ^ "/"

    fun forAll f = let
	  fun for (tm as TM{children, ...}) = (f tm; forChildren(!children))
	  and forChildren [] = ()
	    | forChildren (tm::r) = (for tm; forChildren r)
	  in
	    for
	  end

    fun findTraceModule name = let
	  val n = size name
	  fun find (i, tm as TM{label, children, ...}) = let
		val labelLen = size label
		fun match j = if (j < labelLen)
		        then if (ordof(label, j) = ordof(name, i+j))
			  then match(j+1)
			  else NONE
		      else if (i+j < n)
			then findChild(i+j, !children)
			else SOME tm
		and findChild (i, []) = NONE
		  | findChild (i, c::r) = (case find(i, c)
		       of NONE => findChild(i, r)
			| someTM => someTM
		      (* end case *))
		in
		  if (i+labelLen > n)
		    then NONE
		    else match 0
		end (* find *)
	  in
	    find(0, traceRoot)
	  end

    fun traceModule' (TM parent, name) = let
	  fun checkChildren [] = let
		val tm = TM{
		        full_name = implode[#full_name parent, name, "/"],
		        label = name,
			tracing = ref(!(#tracing parent)),
		        children = ref []
		      }
		in
		  (#children parent) := tm :: !(#children parent);
		  tm
		end
	    | checkChildren((tm as TM{label, ...})::r) =
		if (label = name) then tm else checkChildren r
	  in
	    checkChildren (! (#children parent))
	  end

  (* return the name of the module *)
    fun nameOf (TM{full_name, ...}) = full_name

  (* return the module specified by the given string *)
    fun moduleOf' name = (case findTraceModule(fullName name)
           of NONE => raise NoSuchModule
            | (SOME tm) => tm
          (* end case *))

  (* turn tracing on for a module and its descendents *)
    val traceOn' = forAll (fn (TM{tracing, ...}) => tracing := true)

  (* turn tracing off for a module and its descendents *)
    val traceOff' = forAll (fn (TM{tracing, ...}) => tracing := false)

  (* turn tracing on for a module (but not for its descendents) *)
    fun traceOnly' (TM{tracing, ...}) = tracing := true

  (* return true if this module is being traced *)
    fun amTracing (TM{tracing, ...}) = !tracing

  (* return a list of the registered modules dominated by the given
   * module, and their status.
   *)
    fun status' root = let
	  fun list (tm as TM{tracing, children, ...}, l) =
		listChildren (!children, (tm, !tracing)::l)
	  and listChildren ([], l) = l
	    | listChildren (c::r, l) = listChildren(r, list(c, l))
	  in
	    rev (list (root, []))
	  end

  (** Trace printing **)
    val traceDst = ref TraceToOut

    fun setTraceFile'  t = traceDst := t

    fun tracePrint s = let
	  fun output strm = (CIO.output(strm, s); CIO.flush_out strm)
	  in
	    case !traceDst
	     of TraceToOut => output CIO.std_out
	      | TraceToErr => output CIO.std_err
	      | TraceToNull => ()
	      | (TraceToFile fname) => let
		  val traceDst = (TraceToStream(CIO.open_out fname))
		        handle _ => (
			  CMLBase.reportError(implode[
			      "TraceCML: unable to open \"", fname,
			      "\", redirecting to stdout"
			    ]);
			  TraceToOut)
		  in
		    setTraceFile' traceDst;
		    tracePrint s
		  end
	        | (TraceToStream strm) => output strm
	    (* end case *)
	  end

  (** Trace server **)
    val traceCh : (unit -> string list) chan = channel()
    val traceUpdateCh : (unit -> unit) chan = channel()

    fun traceServer () = let
	  val evt = [
		  wrap(receive traceCh, fn f => tracePrint(implode(f()))),
		  wrap(receive traceUpdateCh, fn f => f())
		]
	  fun loop () = (select evt; loop())
	  in
	    loop()
	  end (* traceServer *)

    fun tracerStart () = (spawn traceServer; ())
    fun tracerStop () = ()

    val _ = (
	  RunCML.logChannel ("TraceCML:trace", traceCh);
	  RunCML.logChannel ("TraceCML:trace-update", traceUpdateCh);
	  RunCML.logServer ("TraceCML:trace-server", tracerStart, tracerStop))

    local
      fun carefully f = if CMLBase.isRunning() then send(traceUpdateCh, f) else f()
      fun carefully' f = if CMLBase.isRunning()
	      then let
	        val reply = condVar()
	        in
	          send (traceUpdateCh, fn () => (writeVar(reply, f())));
		  readVar reply
	        end
	      else f()
    in
    fun traceModule arg = carefully' (fn () => traceModule' arg)
    fun moduleOf name = carefully' (fn () => moduleOf' name)
    fun traceOn tm = carefully (fn () => traceOn' tm)
    fun traceOff tm = carefully (fn () => traceOff' tm)
    fun traceOnly tm = carefully (fn () => traceOnly' tm)
    fun setTraceFile f = carefully (fn () => setTraceFile' f)
    fun status root = carefully' (fn () => status' root)
    end (* local *)

    fun trace (TM{tracing, ...}, prFn) =
	  if (CMLBase.isRunning() andalso (!tracing))
	    then send(traceCh, prFn)
	    else ()


  (** Thread watching **)

  (* controls printing of thread watching messages *)
    val watcher = traceModule (traceRoot, "ThreadWatcher")
    val _ = traceOn watcher

    datatype watcher_msg
      = WATCH of (thread_id * unit cond_var)
      | UNWATCH of thread_id

    val watcherCh : watcher_msg chan = channel ()

  (* watch the given thread for unexpected termination *)
    fun watch (name, tid) = let
	  val cv = condVar()
	  fun handleTermination () = (
		trace (watcher, fn () => [
		  "WARNING!  Watched thread ", name, tidToString tid, " has died.\n"
		  ]);
		send (watcherCh, UNWATCH tid))
	  fun watcherThread () = (
		send (watcherCh, WATCH(tid, cv));
		select [
		    readVarEvt cv,
		    wrap (threadWait tid, handleTermination)
		  ])
	  in
	    spawn (watcherThread); ()
	  end

  (* stop watching the named thread *)
    fun unwatch tid = send(watcherCh, UNWATCH tid)

  (* the watcher server *)
    fun startWatcher () = let
	  fun remove (tid, watchedThreads) = let
		fun look [] = []
		  | look ((id, cv)::r) = if sameThread(tid, id)
		      then (writeVar(cv, ()); r)
		      else ((id, cv) :: look r)
		in
		  look watchedThreads
		end
	  fun loop watchedThreads = (case (accept watcherCh)
		 of (WATCH(arg as (tid, _))) =>
		      loop (arg :: remove (tid, watchedThreads))
		  | (UNWATCH tid) => loop (remove (tid, watchedThreads))
		(* end case *))
	  in
	    spawn (fn () => loop []); ()
	  end

    val _ = (
	  RunCML.logChannel ("TraceCML:watcherCh", watcherCh);
	  RunCML.logServer ("TraceCML:watcher-server", startWatcher, fn () => ()))


  (** Uncaught exception handling **)

    fun defaultHandlerFn (tid, ex) = (
	  CMLBase.reportError (implode [
	      "uncaught exception ", System.exn_name ex, " in thread ",
	      CML.tidToString tid
	    ]))
    val defaultHandler = ref defaultHandlerFn
    val handlers = ref ([] : ((CML.thread_id * exn) -> bool) list)

  (* this sets the default uncaught exception action. *)
    fun setUncaughtFn' action = defaultHandler := action

  (* add an additional uncaught exception action.  If the action returns
   * true, then no further action is taken.  This can be used to handle
   * handle application specific exceptions.
   *)
    fun setHandleFn' action = handlers := action :: !handlers

  (* this resets the default uncaught exception action to the system default,
   * and removes any layered actions.
   *)
    fun resetUncaughtFn' () = (defaultHandler := defaultHandlerFn; handlers := [])

    val exnUpdateCh : (unit -> unit) chan = channel()

    fun exnServerStartup () = let
	  fun handleExn arg = let
		val hdlrList = !handlers and dfltHndlr = !defaultHandler
		fun loop [] = dfltHndlr arg
		  | loop (hdlr::r) = if (hdlr arg) then () else loop r
		in
		  spawn (fn () => ((loop hdlrList) handle _ => (dfltHndlr arg)));
		  ()
		end
	  val event = [
		  wrap (receive exnUpdateCh, fn f => f()),
		  wrap (receive errCh, handleExn)
		]
	  fun server () = (select event; server())
	  in
	    spawn server; ()
	  end

    val _ = (
	  RunCML.logChannel ("TraceCML:exnUpdateCh", exnUpdateCh);
	  RunCML.logServer ("TraceCML", exnServerStartup, fn () => ()))

    local
      fun carefully f = if CMLBase.isRunning() then send(exnUpdateCh, f) else f()
    in
    fun setUncaughtFn arg = carefully (fn () => setUncaughtFn' arg)
    fun setHandleFn arg = carefully (fn () => setHandleFn' arg)
    fun resetUncaughtFn arg = carefully (fn () => resetUncaughtFn' arg)
    end (* local *)

  end; (* TraceCML *)
