(* DebugExec

   This module supports basic user code execution. It coordinates
   and hides the functions of DebugKernel and the various history-maintaining
   subsystems (static,io,signals,exec). Provides for:
   - querying current state when stopped
   - initializing instrumented user code for execution under debugger control.
   - basic record and replay operations
   - CTRL/C handling

   This module can be used directly by higher-level commands, or via
   DebugRun.

*)

signature DEBUG_EXEC =
sig
  type time
  datatype onNoise = QUIET | NOISY | BREAK of (unit->bool)
      (* as in DebugKernel *)
  val initialTime : time ref
      (* time of STARTev for current outermost comp unit. *)
  val finalTime: time ref
      (* time of termination of currently outermost comp unit, or
         infinity if unknown. *)
  val blockingExn: exn option ref
      (* exception blocking current outermost comp unit, if any. *)
  (* State manipulation functions *)
  type state (* ={time:time,
	          cont:userCont,
		  evData:evData,
		  depth:int,
		  memories:doers list (* for each history *)} *)
  val currentState : unit -> state
  val restoreState: (state * onNoise) -> unit
  val zeroState: state  (* the absolutely initial state *)

  (* Querying elements of current state.  These are defined for convenience. *)
  val currentTime: unit -> time
  val currentEvn: unit -> DebugStatic.evn
  val currentLbt: unit -> time
  val currentArgs: unit -> System.Unsafe.object list
  val knownTime: unit -> time

  (* Comp Unit initialization and premature termination. *)
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORT 
    | INTERRUPT
  val setCompUnit: (unit -> 'a) ->
                   (unit -> System.Unsafe.object vector) -> 
                   System.Unsafe.object vector result
  val abortCompUnit: unit ->  'a
  val interruptCompUnit: unit -> 'a 
  val exceptionCompUnit: exn -> 'a

  (* Interpolation *)
  val setInterpolatedUnit: (unit -> unit) -> unit

  (* Interpolation and signal actions. *)
  val forgetActions: unit -> unit

  (* Execution functions *)
  val recordTo: (time * onNoise) -> bool
  val recordRest: unit -> 'a 
  val replayTo: (time * onNoise) -> bool

  (* Query functions *)
  val inCompUnit: unit -> bool

  (* CTRL/C handling *)
  val pendingInterrupt: bool ref
  val setIntHand: unit -> unit
  val resetIntHand: unit -> unit
end

structure DebugExec : DEBUG_EXEC =
struct
  open DebugUtil DebugStatic DebugKernel
  structure U = System.Unsafe
  (* Log for all actions. *)
  structure Log = TimedLog(type entry = action)
  val actionMark = Log.new()
  val zeroActionMark = Log.copyMark actionMark
  val _ = (Log.append actionMark nullAction  (* at time 0 *) ;
	   Log.resetMark actionMark zeroActionMark)
  fun nextAction() : action * time =
      (* Return action for current time (possibly null) and time of first
         logged action after current time. *)
      let val (time,action) = Log.next actionMark
	                          handle Log.Log => (infinity,nullAction)
      in if currentTime() = time then
           (Log.advance actionMark;
	    let val (time',_) = Log.next actionMark
		                  handle Log.Log => (infinity,nullAction)
	    in dbgprint (implode["*act ",
				 makestring (currentTime()), " T ",
				 makestring time',"\n"]);
	       (action,time')
	    end)
         else (dbgprint (implode["*act ",
				makestring (currentTime()), " F ",
				makestring time,"\n"]);
	       (nullAction,time))
      end
  fun rememberActions () =
      let val savedMark = Log.copyMark actionMark
	  fun reset _ = Log.resetMark actionMark savedMark
      in {redo=reset,undo=reset}
      end

  
  (* Master list of subsystem states *)
  val remembers =
      [DebugStatic.rememberEvnTimes,
       DebugStore.remember,
       DebugIO.remember,
       DebugSignals.remember,  
       rememberActions]

  val pendingInterpolation : action option ref = ref NONE
  val initialTime : time ref = ref 0
  val blockingExn : exn option ref = ref NONE
  val finalTime: time ref = ref 0
  val compUnitDepth: int ref = ref 0
  fun inCompUnit () = (!compUnitDepth > 0)

  (* Current state.
     (The implementation at this level is very stateful, because some
     of the history mechanisms must deal with a large current state,
     and it would be quite inefficient to copy this in and out 
     all the time in order to maintain functional cleanliness. *)

  (* Clients may wish to separate out elements of, e.g., evData,
     for efficiency. *)
  type state ={time:time,
	       cont:userCont,
	       evData:evData,
	       depth:int,
	       memories:doers list (* for each history *)}

  fun currentState () : state =
      {time=currentTime(),
       cont=userCont(),
       evData=currentEvData(),
       depth= !compUnitDepth,
       memories=map (fn remember => remember ()) remembers} 

  val zeroState = currentState()

  (* N.B. This must work correctly even when applied to an illicit current
     state (later than the known time). *)
  (*  -- some further checks on legitimacy of the new state might be nice... *)
  fun restoreState ({time,cont,evData,memories,depth},
		    onNoise:onNoise): unit =
    (dbgprint ("*r " ^ (makestring time) ^ "\n");
     let val oldTime = currentTime() 
     in
       if time <> oldTime then
	 (if oldTime < time then
	    app  (fn ({redo,...}:doers) => redo onNoise) memories
	  else app (fn ({undo,...}:doers) => undo onNoise) memories;
	  setCurrentTime time;
	  setUserCont cont;
          setCurrentEvData (evData);
	  compUnitDepth := depth)
       else ()
     end)

  (* Setting up thunks to run. *)

  val currentTime = DebugKernel.currentTime
  fun currentEvn () = #evn(currentEvData())
  fun currentLbt () = #lbt(currentEvData())
  fun currentArgs () = #args(currentEvData())
  val knownTime = DebugKernel.knownTime

  val lastState : state ref = ref (zeroState)
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORT 
    | INTERRUPT
  val resultCont: U.object vector result cont ref = ref(makeCont "resultCont")

  fun reset() = 
      (compUnitDepth := 0;
       restoreState(!lastState,QUIET);
       resetKnownTime();
       finalTime := knownTime())


  (* Routine setCompUnit sets up instrumented code to run, by 
     creating an action to be invoked when user code is re-entered.
     The argument is a compilation unit.
     After setting up the code to run, invokes debugMonitor argument.
     At this point, the other exec routines can be run from monitor 
     commands to record,replay,etc. debugMonitor should return by
     calling a suitable termination command, such as completeCompUnit() 
     or abortCompUnit().
     After recordRest(), abortCompUnit(), or interruptCompUnit()
     has been successfully called, or an uncaught exception is raised,
     setCompUnit returns with result NORMAL, ABORT, INTERRUPT,
     or EXCEPTION, respectively.  On abnormal returns, the current
     state and time will be reset to what they were before the call to
     setCompUnit. It is the caller's responsibility to destroy any 
     copies of the state that refer to an abnormally terminated unit. *)
  fun setCompUnit debugMonitor
                  (f:unit -> U.object vector) : U.object vector result =
    (assert(not (inCompUnit()), "Exec.setCompUnit");
     let fun runUnit () : unit =
	  (if not(inCompUnit()) then
	     inc compUnitDepth
	   else () (* unit is being invoked as an action at tail of 
		      previous unit *);
	   let val result = f() handle exn => 
	                      (exceptionCompUnit exn;
			       debugPanic "returned from exceptionCompUnit")
	   in dec compUnitDepth;
	      advanceKnownTime();
	      finalTime := knownTime();
	      throw (!resultCont) (NORMAL result)
	   end)
     in dbgprint ("*init \n");
        lastState := currentState();
	finalTime := infinity;
	blockingExn := NONE;
	callcc (fn cont => (resultCont := cont;
			    initialTime := currentTime() + 1;
			    Log.append actionMark runUnit;
			    continue(!initialTime,RECORD QUIET,runUnit);
			    assert (knownTime() = !initialTime,
				    "Exec.setCompUnit 2");
			    (* N.B. Must be normal result, ignoring
			     possible interrupt. *)
			    debugMonitor(); 
			    debugPanic "returned from debugMonitor" ))
	before ignore()
     end)

  and exceptionCompUnit exn =
      (pseudoEvent{evn=pseudoEvn UNCAUGHTev,
		   forced=true,
		   args=[U.cast exn]};
       reset();
       throw (!resultCont) (EXCEPTION exn))

  fun abortCompUnit() = 
      (reset();
       throw (!resultCont) ABORT)

  fun interruptCompUnit() = 
      (reset();
       throw (!resultCont) INTERRUPT)

  (* An interpolated comp unit is set to run as an action starting at
     the current time. When the interpolation returns normally, 
     the previous code will be resumed.
     Setting an interpolation removes all previous interpolations set
     at the current or any later time.  It is the caller's responsibility to
     destroy any copies of the state that refer to any later time.
     On abnormal results the current state and time will be reset to what 
     they were before the *outer-level call* to setCompUnit, and the
     *outer-level* abnormal return will be made immediately. *)
  fun setInterpolatedUnit (f:unit -> unit) : unit =
      (assert (inCompUnit(),"Exec.interpolateCompUnit 0");
       assert (currentTime() < !finalTime,"Exec.interpolateCompUnit 1");
       let fun runUnit () : unit =
	   (inc compUnitDepth;
	    DebugSignals.permitSignals false;
	    f();
	    DebugSignals.permitSignals true;
	    dec compUnitDepth)
       in dbgprint ("*initInterp " ^ (makestring (!compUnitDepth)) ^ "\n");
 	  finalTime := infinity;
	  blockingExn := NONE;
	  resetKnownTime();
	  pendingInterpolation := SOME runUnit  (* replaces any previous *)
       end)

  fun getInterpolation() : action option =
      case !pendingInterpolation of
	SOME action => (pendingInterpolation := NONE;
			SOME action)
      | NONE => NONE

  fun forgetActions() : unit  =
     (pendingInterpolation := NONE;
      DebugSignals.forgetSignals())

  (* CTRL/C (SIGINT) handling. *)
  val pendingInterrupt = ref false
  local
    open System.Signals
    type handler = (int * unit cont) -> unit cont
    val handlerSet = ref false
    val normalHandOpt = ref (NONE:handler option)
    fun debugHand (cnt,cont) =
	(if not(!pendingInterrupt) then
	   (pendingInterrupt := true;
  	    setTargetTime (currentTime() + 1))
	 else ();
	 cont)
  in 
    fun setIntHand () =
	if not (!handlerSet) then
	     (normalHandOpt := inqHandler(SIGINT);
	      setHandler(SIGINT,SOME(debugHand));
	      handlerSet := true;
      	      pendingInterrupt := false)
	else ()

    fun resetIntHand () =
	if !handlerSet then
	  (setHandler(SIGINT,!normalHandOpt);
	   handlerSet := false)
	else () (* debugPanic "resetIntHand" *)
  end

  (* Fundamental execution functions. *)
  (* Return conditions:
     When we return from recordTo or replayTo, at *least* one of 
     the following will be true:
      (a) We reached target, in which case currentTime = target.
      (b) We hit end of program or interpolation.
      (c) We hit an uncaught exception.
      (d) We hit a noise event with onNoise=BREAK (true condition). 
      (e) CTRL/C was hit, in which case pendingInterrupt will be true. 
      (f) A halting signal occurred during recording, in which case 
                DebugSignals.deliverableHalting will return true and
		deliverableSignal will return the signal.
     Note that (b),(c),(d) are mutually exclusive, but (a),(e), and
     (f) can occur in combination with any of the others and/or each
     other. Note also that on replayTo, (b) and (c) can be true only
     if the target argument = knownTime(), and (f) cannot occur.

     The distinctions among (b),(c), and (d) are important for setting
     finalTime and blockingExn, but are not generally of interest to
     callers.  So for right now, we return just a boolean, which will
     be false iff one of (b),(c),(d), occured.  Callers must still
     check for (e) by consulting pendingInterrupt and, when recording, 
     for (f) by calling deliverableHalting().
    
     The following datatype is used internally, and may prove useful
     externally in future. *)

  datatype execResult = 
      EXEC_NORMAL | EXEC_END | EXEC_EXN of exn | EXEC_NOISE | EXEC_INTERP

  (* Analyse current state immediately after returning from execution.
     Not for export. *)
  fun execResult onNoise : execResult =
      case (hd o eventsFor) (currentEvn()) of
	ENDev _ => 
	  if (knownTime() = currentTime()) then (* avoiding old ENDevs *)
	    if (!compUnitDepth > 1) then (* interpolation ENDev*)
	      EXEC_INTERP
	    else EXEC_END
	  else EXEC_NORMAL
      | UNCAUGHTev => EXEC_EXN (U.cast (hd(currentArgs())))
      | IOev => (case onNoise of
	          BREAK condition => if condition() then 
		                       EXEC_NOISE
				     else EXEC_NORMAL
	 	 | _ => EXEC_NORMAL)
      | _ => EXEC_NORMAL

  fun getAction () : action =
      (* Check if an interpolation or signal handle action should be
         injected before recording from the current time. 
         Interpolations have priority over signals.
	 If an action is chosen, it is added to the action log. *)
      case(case getInterpolation() of
	     SOME action => SOME action
	   | NONE => DebugSignals.handleSignal()) of
	SOME action => (Log.append actionMark action;
			(* N.B. implicitly zaps later log entries *)
			action)
      | NONE => nullAction

  fun recordTo (target:time,onNoise:onNoise) : bool =
      (* Record up to target time, unless something else intervenes.
         Check pending actions first.
         For possible conditions on return, see above. *)
      (dbgprint ("*rec " ^ makestring(target) ^ "\n");
       (* assert (inCompUnit(),"Exec.recordTo"); *)
       assert(currentTime() = knownTime(),"Exec.recordTo 2");
       assert(target > knownTime(),"Exec.recordTo 3");
       let fun go () = 
	     if currentTime() < !finalTime then
	       ((* jump into user program *)
		continue(target,RECORD onNoise,getAction()); 
		(* see why we stopped *)
		case execResult onNoise of
		  EXEC_END =>
		    (finalTime := knownTime(); false)
		| EXEC_INTERP => false
		| EXEC_EXN exn => 
		    (finalTime := knownTime(); blockingExn := SOME exn; false)
		| EXEC_NOISE => false
		| EXEC_NORMAL =>
		    if target = knownTime() orelse !pendingInterrupt 
		       orelse DebugSignals.deliverableHalting () then
		      true
		    else (* presumably false noise break or 
			    non-halting deliverable signal *)
		      go ())
	     else false
       in go ()
       end)

  fun recordRest ()  =
      (* Record remainder of compilation unit, jumping back to caller
         of the unit when done. 
	 This function does not return. *)
      (dbgprint ("*recr\n");
       (* assert (inCompUnit(),"Exec.recordRest 1"); *)
       assert(currentTime() = knownTime(),"Exec.recordRest 2");
       (* assume associated state appropriately set too *)
       let fun go () =
             ((* jump into user program *)
	      continue(infinity,RECORD NOISY,getAction()); 
	      if !pendingInterrupt then
		interruptCompUnit() (* does not return *)
	      else (* presumably non-halting deliverable signal *)
	        go ())
       in go() (* does not return *)
       end)
    
  fun replayTo (target:time,onNoise:onNoise) : bool =
    (* For possible conditions on return, see above.
       If none of these conditions are met and we cannot reach 
       target time for any reason, raise an exception.
       We handle actions internally. *)
      (dbgprint ("*repl " ^ makestring(target) ^ "\n");
       (* assert (inCompUnit(),"Exec.replayTo 1"); *)
       assert (target <= knownTime(),"Exec.replayTo 2");
       assert (target > currentTime(),"Exec.replayTo 3");
       (* assume state for currentTime appropriately set too *)
       let fun loop() =
	   let val (action,nextActionTime) = nextAction()
	       val target' = min(target,nextActionTime)
	   in (currentTime() < !finalTime) 
	      andalso
	      (continue(target',REPLAY onNoise,action);
	       case execResult onNoise of
		 EXEC_NORMAL =>
		     (!pendingInterrupt)
		     orelse 
		     (currentTime() = target) 
		     orelse
		     loop()
	       | _ => false)
	   end
       in loop()
       end)
end
