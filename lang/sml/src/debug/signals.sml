(* DebugSignals
   Support "historical" signals.
*)

signature SIGNALS =
  sig
    datatype signal
      = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGTERM | SIGURG
      | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2 | SIGVTALRM 
      | SIGPROF | SIGTSTP | SIGCONT (* not yet supported *)
      | SIGGC
    val setHandler : (signal * ((int * unit cont) -> unit cont) option) -> unit
    val inqHandler : signal -> ((int * unit cont) -> unit cont) option
    val maskSignals : bool -> unit
    val pause : unit -> unit
	(* sleep until the next signal *)
  end

signature DEBUG_SIGNALS = 
sig
  (* user-level functions *)
  include SIGNALS 
  (* debugger-control functions *)
  val remember: unit -> DebugKernel.doers
  val deliverableSignal: unit -> (signal*int) option
      (* return current deliverable signal, if any *)
  val handleSignal : unit -> DebugKernel.action option
      (* return action to hanlde current deliverable signal, if any *)
  val setSignal: signal -> unit (* add signal to pending vector *)
  val clearSignal: signal -> unit (* remove signal from pending vector *)
  val getSignal : signal -> int (* get current pending count *)
  val forgetSignals : unit -> unit (* clear all signal counts *)
  val deliverableHalting : unit -> bool (* should execution halt on the
					   deliverable signal? *)
  val setHalting :signal * bool -> unit (* set halting status *)
  val permitSignals : bool -> unit  (* control inhibition flag *)
end

structure DebugSignals : DEBUG_SIGNALS =
struct
  open Array List DebugUtil DebugKernel DebugStatic
  infix 9 sub

  fun some x = x <> NONE

  (* are we recording? *)
  fun recording () : bool = 
      case !execMode of
	RECORD _ => true
      | _ => false

  structure S = System.Signals
  open S
  val nsigs = 16
  exception UnimplementedSignal

  (* Convert SML signal names to run-time signal codes.  
     For convenience, these should agree with S.Signals. *)
  fun sig2code SIGHUP    = 0
    | sig2code SIGINT    = (* 1 *) raise UnimplementedSignal
    | sig2code SIGQUIT   = 2
    | sig2code SIGALRM   = 3
    | sig2code SIGTERM   = 4
    | sig2code SIGURG    = 5
    | sig2code SIGCHLD   = 6
    | sig2code SIGIO	 = 7
    | sig2code SIGWINCH  = 8
    | sig2code SIGUSR1   = 9
    | sig2code SIGUSR2   = 10
    | sig2code SIGTSTP   = (* 11 *) raise UnimplementedSignal
    | sig2code SIGCONT   = (* 12 *) raise UnimplementedSignal
    | sig2code SIGGC	 = 13
    | sig2code SIGVTALRM    = 14
    | sig2code SIGPROF      = (* 15 *) raise UnimplementedSignal

  fun code2sig 0  = SIGHUP
    | code2sig 1  = (* SIGINT *) raise UnimplementedSignal
    | code2sig 2  = SIGQUIT
    | code2sig 3  = SIGALRM
    | code2sig 4  = SIGTERM
    | code2sig 5  = SIGURG
    | code2sig 6  = SIGCHLD
    | code2sig 7  = SIGIO
    | code2sig 8  = SIGWINCH
    | code2sig 9  = SIGUSR1
    | code2sig 10 = SIGUSR2
    | code2sig 11 = (* SIGTSTP *) raise UnimplementedSignal
    | code2sig 12 = (* SIGCONT *) raise UnimplementedSignal
    | code2sig 13 = SIGGC
    | code2sig 14 = SIGVTALRM
    | code2sig 15 = (* SIGPROF *) raise UnimplementedSignal
    | code2sig _ = debugPanic "signals.code2sig"

  type handler = (int * unit cont) -> unit cont

  (* number of signals of each type pending. *)
  val pendvec = array(nsigs,0)  

  (* flag : signals permitted *)
  val signalsOK = ref true

  (* flag: are we executing a user signal handler now? *)
  val inHandler = ref false

  (* Our simulation of masking. Note that since SIGINT must never 
     be masked indefinitely, we don't use S.maskSignals here. *)
  val maskLevel = ref 0  

  (* is signal delivery enabled? *)
  fun deliveryOK () = !signalsOK andalso 
                           !maskLevel = 0 andalso not (!inHandler) 

  (* is there a deliverable signal? *)
  fun deliverableSignal() : (signal*int) option =
      let fun loop code = 
	  let val cnt = pendvec sub code
	  in if cnt > 0 then
	       SOME(code2sig code,cnt) 
	              handle UnimplementedSignal => loop(code+1)
	     else loop(code+1)
	  end handle Subscript => NONE
      in if deliveryOK() 
	   then loop 0
         else NONE
      end

  (* Force halt at next time step; idempotent function *)
  fun signalHalt() = setTargetTime(currentTime() + 1) 

  fun checkSignalHalt() = 
      if recording() andalso some(deliverableSignal()) then
	signalHalt()
      else ()

  (* Internal masking *)
  fun permitSignals false = signalsOK := false
    | permitSignals true = 
         (signalsOK := true;
	  checkSignalHalt())

  (* User-level masking *)
  fun maskSignals true = inc maskLevel
    | maskSignals false = 
	  if !maskLevel > 0 then 
   	    (dec maskLevel;
	     checkSignalHalt())
	  else ()

  (* Install true handler for all implemented signals.
     This handler records signals received during recording.
     If signals are not masked, the target time is reset to
     force recording to halt at the next time-step.
     N.B. It would be nice to do something about IO here; i.e., to
     emulate the approach taken by the lower level to allow IO
     waits to be interruptible.  Unfortunately, I don't see any way
     to do this without exposing/simulating the inards of the IO package. *)

  fun trueHandler code (cnt,cont) =
       (if recording() then
	  (update(pendvec,code,pendvec sub code + cnt);
	   if deliveryOK() then
	     signalHalt()
	   else ())
	else ();
	cont)

  (* We always keep true handler installed for termination signals, i.e., those
     assigned default action DFL_TERM_NO_CORE in signal.c (except SIGINT,
     which debugger programs are not allowed to use). 
     If these signals occur when no handler is present, an empty default
     handler will be used; by default they will cause a halt.
     Other signals get true handler only when there is a user handler
     installed for them, thus giving the default action assigned
     in signal.c.  In particular, this means SIGQUIT will cause the
     debugger system to die if no user handler is installed, and
     SIGURG, SIGCHLD, SIGIO, SIGWINCH, and SIGGC will be ignored.
     The user can still invoke these signals manually, in which case
     they will also be handled by the empty default handler,
     and, by default, cause a halt. *)
  val keepTrueHandler = [SIGHUP,SIGALRM,SIGTERM,SIGUSR1,SIGUSR2,SIGVTALRM]
  val _ = 
    app (fn signal => S.setHandler(signal,SOME(trueHandler (sig2code signal))))
        keepTrueHandler

  (* Explicit pendvec control.  These functions should be called when
     execMode is not RECORD to avoid access conflicts on pendvec. *)
  fun setSignal signal : unit = 
      let val code = sig2code signal
      in if pendvec sub code = 0 then
	   resetKnownTime()
         else();
         update(pendvec,code,pendvec sub code + 1)
      end
      
  fun clearSignal signal : unit =
      let val code = sig2code signal
      in if pendvec sub code > 0 then
	   resetKnownTime()
	 else();
	 update(pendvec,code,0)
      end

  fun getSignal signal : int = 
      let val code =sig2code signal
      in pendvec sub code
      end

  fun forgetSignals () : unit = ArrayExt.refill(pendvec,0)

  (* the user's handlers *)
  val sigvec = array(nsigs,NONE:handler option)

  (* user version *)
  fun setHandler (signal,handler) = 
       let val code = sig2code signal (* may raise *)
       in update(sigvec,code,handler);
	   case handler of
	     SOME _ => 
	       S.setHandler(signal,SOME (trueHandler code))
	   | NONE => 
	       if not (exists (fn h => sig2code h = code) keepTrueHandler) then
		 S.setHandler(signal,NONE)
	       else ()
       end

  fun saveHandlers () =  ArrayExt.copy sigvec

  fun resetHandlers (newsigvec:handler option array) =
      let fun loop n =
	    let val newh = newsigvec sub n
	    in setHandler(code2sig n,newh)
	                   handle UnimplementedSignal => ();
	       loop (n+1)
	    end handle Subscript => ()
      in loop 0
      end

  fun inqHandler signal = sigvec sub (sig2code signal)

  (* Track which signals should produce execution halts.
     By default, halt iff there is no installed user handler;
     user cna explicitly override default with setHalting. *)
  val haltvec:bool option array = array(nsigs,NONE)
  fun setHalting (signal,state:bool) : unit =
      let val code = sig2code signal
      in update(haltvec,code,SOME state)
      end
  fun deliverableHalting () : bool =
      case deliverableSignal() of
	SOME(signal,_) => 
	    let val code = sig2code signal
	    in case haltvec sub code of
	         SOME state => state
	       | NONE => (case sigvec sub code of
			    SOME _ => false
			  | NONE => true)
	    end
      | NONE => false

  fun pause () =
      (* N.B. This doesn't work very well wrt/atomicity, but neither
         does the built-in version! *)
    case (!execMode) of
      RECORD _ => S.pause ()
    | REPLAY _ => ()
    | STOP => S.pause ()


  (* Return action to handle current deliverable signal, if any.  
     Successful call clears pending signal. *)
  fun handleSignal ()  =
     case deliverableSignal() of
       SOME (signal,cnt) => 
	   let val code = sig2code signal
	       fun handlerAction () : unit =
		   (dbgprint (implode["*sighandle ",makestring code," ",
					makestring(currentTime()), "\n"]);
		    inHandler := true;
		    let val startTime = (pseudoEvent{evn=pseudoEvn SIGSTARTev,
						     forced=false,
						     args=nil};
					 currentTime())
		    in callcc(fn cont =>
			     let val cont' = 
				 case sigvec sub code of
				     SOME userHandler => 
					userHandler(cnt,cont)
				   | NONE => cont (* empty handler *)
			     in inHandler := false;
				(* set up for possible further signals *)
				checkSignalHalt();
				throw cont' ()
			     end);
			pseudoEvent{evn=pseudoEvn SIGENDev,
				    forced=false,
				    args=[System.Unsafe.cast startTime]}
		    end)
	   in update(pendvec,code,0); (* don't worry about access conflicts *)
	      SOME handlerAction
	   end
     | NONE => NONE

  fun remember() =
    let val savedHandlers = saveHandlers()
        val savedLevel = !maskLevel
	val savedInHandler = !inHandler
	fun reset _ = 
	    (resetHandlers savedHandlers; 
	     maskLevel := savedLevel;
	     inHandler := savedInHandler)
    in {redo=reset,undo=reset}
    end

end



