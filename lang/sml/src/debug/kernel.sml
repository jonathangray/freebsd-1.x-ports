(* DebugKernel

   Fundamentals of executing user code under debugger control.
   Handles transfer of control between debugger and user program,
   execution mode for user program, and "current state" of user
   program when it is stopped.

   This interface is used directly only by DebugExec and the various
   history-maintaining subsystems (Store,Io,Signals).
   Instrumented code also needs access to times and break; this 
   is provided via a special system ref, set in DebugInterface. *)


signature DEBUG_KERNEL =  
sig
  (* Basic types *)
  type time (* = int *)
  type evn (* = int *)
  type action (* = unit -> unit *)
  type userCont (* = action cont *)

  val nullAction: action

  datatype onNoise = QUIET | NOISY | BREAK of (unit->bool)
      (* Control what happens when a visible side-effect occurs:
         we may want to muffle them (QUIET),
         perform them under all circumstances (NOISY),
	 or re-enter debugger control with a pseudo-event (BREAK).
	 We ignore the condition argument to BREAK at this level.*)
  type doers (* = {redo:onNoise->unit,undo:onNoise->unit} *)
       (* Subsystem history mechanism encapsulates a remembered state in
	  this type:
	   redo(onNoise) will redo to remembered state from (earlier) 
	     current state;
	   undo(onNoise) will undo to remembered state from (later) 
	     current state. *)

  (* Describes the mode in which user code is executed.  This is chiefly
     useful for history-maintaining subsystems. 
     Mode IGNORE should be set when the user program is not supposed
     to have control.  It is necessary because higher levels of the system
     do not provide any way to prevent instrumented user code from being
     run outside of the debugger's control; if this happens, we want to
     pretend that the instrumentation is not there, as far as we can.
     Note that IGNORE should be set to prevent unwanted signals while 
     the debugger has control but is not executing the user program.  *)
  datatype execMode = RECORD of onNoise
                    | REPLAY of onNoise
		    | IGNORE (* not running under debugger control *)
  val execMode: execMode ref  (* current mode *)

  (* Data returned by user program when it transfers control back to debugger. *)
  type evData  (* = {evn:evn,lbt:time,args:System.Unsafe.object list} 
                     event number, last bind time, arguments *)

  (* Accessing elements of current state. *)
  val times: time array  (* only exported for DebugInstrum; 
			    others: don't user directly!! *)
  val currentTime: unit -> time 
  val setCurrentTime: time -> unit 
  val targetTime: unit -> time   
  val setTargetTime: time -> unit 
  val knownTime: unit -> time
  val advanceKnownTime: unit -> unit (* to current time, if later. *)
  val resetKnownTime: unit -> unit  (* to current time, if earlier. *)
  val userCont: unit -> userCont
  val setUserCont: userCont -> unit
  val currentEvData: unit  -> evData
  val setCurrentEvData: evData -> unit

  (* Transfering control between debugger and user program *) 
  val continue: time * execMode * action -> unit 
       (* Transfer control to user program, executing action
	  before returning to instrumented code execution.
	  Returns when control transfered back to debugger. *)
  val ignore: unit -> unit
       (* Set mode and target time so that instrumented code can be
	  executed harmlessly outside of debugger control. *)
  val break: System.Unsafe.object array -> unit  
       (* Return control to debugger from instrumented code. *)
  val pseudoEvent: {evn:evn,forced:bool,args:System.Unsafe.object list} -> unit
       (* Return control to debugger from special built-in code. *)
  val execTime : time ref  (* total execution steps *)
end

structure DebugKernel: DEBUG_KERNEL =
struct
  open Array List DebugUtil 
  infix 9 sub
  type time = int
  type evn = int
  type action = unit -> unit
  val nullAction = fn () => ()
  type userCont = action cont
  datatype onNoise = QUIET | NOISY | BREAK of (unit->bool)
  type doers  = {redo:onNoise->unit,undo:onNoise->unit}
  datatype execMode = RECORD of onNoise
                     | REPLAY of onNoise
		     | IGNORE (* not running under debugger control *)
  type evData = {evn:evn,lbt:time,args:System.Unsafe.object list} 
  val execMode = ref IGNORE
  val times = array (2,0)	(* sub 0 = currentTime *)
				(* sub 1 = targetTime *)
  fun currentTime () = times sub 0
  fun setCurrentTime t = update(times,0,t)
  fun targetTime () = times sub 1
  fun setTargetTime t = update(times,1,t)
  local 
    val knownTimeR = ref 0
  in
    fun knownTime() = !knownTimeR
    fun resetKnownTime() = 
	if !knownTimeR >= currentTime() then
	  knownTimeR := currentTime()
        else debugPanic (implode["kernel.resetKnownTime ",
				 makestring (currentTime()), " ",
				 makestring (!knownTimeR)])
    fun advanceKnownTime() =
	if !knownTimeR < currentTime() then
	  knownTimeR := currentTime()
	else ()
  end
  val nullEvData = {evn=0,lbt=0,args=nil:System.Unsafe.object list}
  local
    val currentEvDataR = ref nullEvData
  in
    fun currentEvData () = !currentEvDataR
    fun setCurrentEvData evData = currentEvDataR := evData
  end

  val startTime = ref 0
  val execTime = ref 0

  val debugContR:unit cont ref = ref (makeCont "debugCont")
  val userContR: userCont ref  = 
      ref (callcc (fn c => ((callcc (fn c' => throw c c')) ();
			    debugPanic ("empty userCont"))))
  fun userCont () = !userContR
  fun setUserCont c = userContR := c

  fun continue (target:time, mode:execMode, action:action) =
      (setTargetTime target;
       execMode := mode;
       startTime := currentTime();
       callcc (fn cont => 
	       (debugContR := cont;
		throw (!userContR) action)))

  fun ignore () =
      (execMode := IGNORE;
       setTargetTime infinity)

  (* called from instrumented code *)
  fun break (stuff:System.Unsafe.object array) =
      callcc(fn cont =>  (ignore();
			  userContR := cont;
			  setCurrentEvData 
			       {evn=System.Unsafe.cast (stuff sub 0),
				lbt=System.Unsafe.cast (stuff sub 1),
				args=
				  let fun makelist n =
					 (Array.sub(stuff,n)::(makelist (n+1)))
						  handle Subscript => nil
				  in makelist 2
				  end};
			  execTime := 
			      ((!execTime + (currentTime() - (!startTime)))
			         handle Overflow => 0);
			  advanceKnownTime();
			  throw (!debugContR) ())) ()

  (* called from predefined code *)
  fun pseudoEvent {evn:evn,forced:bool,args:System.Unsafe.object list} : unit =
    (setCurrentTime (currentTime() + 1);
     if forced orelse currentTime() = targetTime() then
       callcc (fn cont => 
	          (ignore();
		   userContR := cont;
		   setCurrentEvData
		   {evn=evn,
		    lbt=currentTime()-1,
		    args=args};
		   execTime := 
		   !execTime + (currentTime() - (!startTime));
		   advanceKnownTime();
		   throw (!debugContR) ())) () 
     else ())
end
