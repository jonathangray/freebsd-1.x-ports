(* DebugMotions

   Support for breakpoints, motion forward/backward to nearest breakpoint, and
   more general motions.  Intended for use directly by user-mode interface or
   interactive system interface.

   These commands have a notion of established time/state; that is, the 
   time/state last reached by executing one of them.  Since the interactive
   system may allow instrumented user code to execute independently of 
   the debugger, so altering the current time/state, it is important to
   reset to the established time/state before doing anything else.  These
   commands all do that, and there is also a wrapping function suitable for
   use by other commands, e.g., those in DebugQueries. *)


signature DEBUG_MOTIONS = 
sig
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORT
    | INTERRUPT
  val runCompUnit: (unit -> 'a) ->
                   (unit -> System.Unsafe.object vector) -> 
                     System.Unsafe.object vector result
      (* Set thunk to run as next comp unit. *)
  val interpolateCompUnit : (unit -> unit) -> unit
      (* Set thunk to be interpolated at current time. *)
  type time
  type place
  type wherewhen (* = place * time *)
  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING
  val jump: time -> wherewhen outcome (* to specified time *)
  val binSearch: (unit->time) * time * bool  -> wherewhen outcome
      (* First argument evaluates a condition, returning
         the first time it is known to be true (infinity if it isn't so known).
	 Second argument is an absolute limit.  
	 Binary search forward to the first time when the condition
	 evaluates < infinity, or the limit is reached. 
	 If an interrupt or halting signal occurs, returns immediately with
	 current time at an arbitrary value > = inital time,
	 unless third argument is true, in which case 
	 resets to initial time before returning. *)
  val complete: unit -> unit outcome  (* returns only if NOTRUNNING *)
  val abort: unit -> unit outcome  (* returns only if NOTRUNNING *)
  val withEstablishedTime: (time->'a) -> 'a outcome
      (* Evaluate first arg, giving it established time as arg, and
         guaranteeing a return to established time when done. *)
  val keepEstablishedTime: (unit->'a) -> 'a outcome (* ?? *)
  val cpCost: int ref
  val setHandler: (DebugSignals.signal * ((int * unit cont) -> unit cont) option) -> unit
  val setSignal: DebugSignals.signal -> unit
  val clearSignal: DebugSignals.signal -> unit
end

structure DebugMotions : DEBUG_MOTIONS =
struct
  open DebugExec DebugRun DebugUtil DebugStatic
  structure U = System.Unsafe
  val establishedState: state ref = ref (zeroState)
                         (* the state we want to appear to be in *)

  fun establishState() = 
      (dbgprint ("*e " ^ makestring(currentTime()) ^ "\n");
       establishedState := currentState())

  (* All commands that assume current state/time
  should restoreState of establishedState before they start.
  All commands performing explicit time travel should keep 
  establishedState sensible as they complete.
  *)
  
  fun runCompUnit (debugMonitor:unit -> 'a)
                  (f:unit -> U.object vector) : U.object vector result =
      let val result = (setIntHand(); 
			setCompUnit 
			    (fn () => (resetIntHand();
				        (* ignore possible interrupt *)
				       establishState();
				       debugMonitor()))
			    f)
      in zapRun(currentTime());
	 establishState();
	 resetIntHand();
	 result
      end
  
  fun interpolateCompUnit (f:unit -> unit) : unit =
     (setIntHand(); 
      setInterpolatedUnit f;
      zapRun(currentTime());
      establishState();
      resetIntHand())
  
  (** All commands are responsible for polling interruptPending at sensible
  times, including but not limited to when they have just come back from
  a time-travel command, perhaps prematurely. 
  Moreover, commands are responsible for setting/clearing the 
  debugger's interrupt handler, so that it is set during polling periods. *)
  
  datatype 'a outcome = COMPLETED of 'a
                      | INTERRUPTED of 'a
		      | NOTRUNNING

  type wherewhen = place * time
    
  (* utility functions *)
  
  fun currentWhereWhen () = (hd(placesFor(currentEvn())),currentTime())
  
  (* suitable for commands that may change established time. *)
  fun checkNotInterrupted f =
    if inCompUnit() then
      (setIntHand();
       restoreState (!establishedState,QUIET);
       let val r = f()
       in resetIntHand();
	  if !pendingInterrupt then
	    INTERRUPTED r
	  else COMPLETED r
       end)
     else NOTRUNNING
  
  (* suitable for commands that do time-travel but don't want to change
     established time. *)
  fun keepEstablishedTime f =
    checkNotInterrupted (fn () => (f() before 
				   restoreState(!establishedState,QUIET)))

  (* suitable for invoking a function with the current established time. *)
  fun withEstablishedTime f =
    keepEstablishedTime (fn () => f(currentTime()))
  
  fun complete() : unit outcome =  (* returns only if NOTRUNNING *)
    checkNotInterrupted(completeCompUnit)
  
  fun abort() : unit outcome = (* returns only if NOTRUNNING *)
    checkNotInterrupted(abortCompUnit)
  
  fun jump (target:time) : wherewhen outcome =
    checkNotInterrupted(fn () =>
     let val target = min(max(target,!initialTime),!finalTime)
     in
       if target < currentTime() then
	 (forgetActions();
	  resetTo (target,true,QUIET); 
	  (* if we were interrupted, pretend we went nowhere. *)
	  if target = currentTime() then
	    establishState()
          else restoreState(!establishedState,QUIET))
       else (advanceTo (target,true,NOISY); (* possibly interrupted 
					     or signalled*)
	     establishState());
       currentWhereWhen()
     end)

  val cpCost = ref 4000
  fun maxNarrowDelta()  = !cpCost * 8 (* here we know odds are higher ?? *)
  fun maxExpandDelta() = !cpCost *  8
  fun initExpandDelta() = !cpCost 

  fun binSearch(condition:unit->time,maxtime:time,reset:bool): wherewhen outcome =
       (* Evaluating condition on unit returns an upper bound on the time when
	  desired state has been reached -- infinity if no such bound yet known.
	  Execute forward until
	    - first time for which condition() < infinity, and/or
	    - maxtime is reached, and/or
	    - termination is reached, and/or
	    - interrupted or signalled (while recording). 
	  Will do quiet binary search for condition. *)
    checkNotInterrupted(fn () =>
      let val _ = dbgprint ("*bs " ^ (makestring maxtime) ^ "\n");
	  fun bcond () = condition() <= currentTime()
	  val inittime = currentTime()
	  val maxtime = min(maxtime,!finalTime)
	  fun narrow (ftime,ltime,onNoise) =
	   (dbgprint ("*nar " ^ (makestring ftime) ^ " " ^
		              (makestring ltime) ^ "\n");
	    if (ftime < ltime) then
	       (let val target = (ftime + ltime) div 2
		    val target = min(target,ftime+(maxNarrowDelta()))
		in resetTo (target,false,onNoise);
		   if !pendingInterrupt then
		     () 
		   else
		     let val minc = condition()
		     in if minc < DebugUtil.infinity then
			  narrow (ftime,minc,QUIET)
			else narrow (target+1,ltime,onNoise)
		     end
		end)
	    else resetTo (ltime,false,QUIET))  (* interrupt may be raised *)
	  fun expand delta = 
	    let val delta' = min(delta,maxExpandDelta())
		val startTime = currentTime()
		val target = min(currentTime() + delta',maxtime)
	    in dbgprint ("*exp " ^ (makestring startTime) ^ " " ^
			           (makestring target) ^ "\n");
	       advanceTo(target,false,BREAK bcond);
	       let val minc = condition()
		   val maxtime = min(maxtime,!finalTime)
	       in if !pendingInterrupt orelse 
		     DebugSignals.deliverableHalting() then
		    ()
		  else if (minc < DebugUtil.infinity) then
		    narrow(startTime+1,minc,QUIET)
		  else if (currentTime() < maxtime) then
		    expand((currentTime() - startTime) * 2 + 1)
		  else ()
	       end
	    end
	  val (ftime,ltime) = bracketCache bcond
      in
	dbgprint ("*bra " ^ (makestring ftime) ^ " " ^ 
		            (makestring ltime) ^ "\n");
	if maxtime < ftime then
	  advanceTo (maxtime,true,NOISY)
	else 
	  (advanceTo (ftime,false,NOISY);
	   if !pendingInterrupt orelse DebugSignals.deliverableHalting() then
	    ()
	   else if ltime < infinity then 
	     narrow(ftime,ltime,BREAK bcond)
	   else
	     expand (initExpandDelta()));
	(* If an interrupt or signal occured there may(?) be a small 
	   chance that we've actually gone backwards! 
	   If so, pretend we went nowhere. 
	   We might want to distinguish more cases here depending on caller.*)
	if currentTime() < inittime orelse
 	   (reset andalso (!pendingInterrupt orelse 
			   DebugSignals.deliverableHalting())) then
	  restoreState (!establishedState,QUIET)
	else
	  establishState();
	currentWhereWhen()
      end)
  
  (* This version of setHandler alters the installed handler so that 
     uncaught exceptions raised within the handler are immediately 
     returned uncaught to top level, avoiding any pending user handlers. 
     It is defined here because it needs exceptionCompUnit. *)
  fun setHandler(signal,handleropt) = 
      let val safeHandler =
	  case handleropt of
	    SOME handler =>
	       SOME (fn x => (handler x) handle exn => exceptionCompUnit exn)
	  | NONE => NONE
      in DebugSignals.setHandler(signal,safeHandler)
      end

  fun setSignal(signal:DebugSignals.signal) : unit =
      (zapRun(currentTime());
       DebugSignals.setSignal signal)

  fun clearSignal(signal:DebugSignals.signal) : unit =
      (zapRun(currentTime());
       DebugSignals.clearSignal signal)

end (* structure DebugMotions *)
