(* User-level debugger code. *)
structure UserDebugCommands : 
sig
   type int
   type string
   type unit
   type 'a option
   type 'a ref
   val selectCurrent : unit -> unit
   val selectBackTrace : unit -> unit
   val selectNear: string -> int -> unit
   val selectNext: unit -> unit
   val selectPrev: unit -> unit
   val showEvents: string -> unit
   val breakWhen: int -> int
   val breakNear: (string * int * int) -> int option
   val deleteBreak: int -> unit
   val clearBreaks: unit -> unit
   val showBreaks: unit -> unit
   val showBreakTimes: unit -> unit
   val toggleBreak: unit -> unit
   val bfunc: (unit -> unit) -> unit
   val tfunc: int -> (unit -> unit) -> unit
   val nofunc: int -> unit
   val current : unit -> unit
   val ss : unit -> unit
   val ssb : unit -> unit
   val forward: unit -> unit
   val backward: unit -> unit
   val jump: int -> unit
   val sk: unit -> unit
   val skb: unit -> unit
   val jumpTrace: unit -> unit
   val showCalls: int -> unit
   val upCall: unit -> unit
   val downCall: unit -> unit
   val showVal: string -> unit
   val emacsShowVal: string -> unit
   val complete: unit -> unit
   val abort: unit -> unit
   val usedbg: string -> unit
   val usedbg_live: string -> unit
   val usedbg_stream: instream -> unit
   val usedbg_string : string -> unit
   val usedbg_script: instream -> instream -> unit
   val run: string -> unit
   val execute: string -> unit
   val debugdebug: bool -> unit
   val emacsInit: unit -> unit
   val cd: string -> unit
   val haltOnSignal: System.Signals.signal * bool -> unit
   val setSignal: System.Signals.signal -> unit
   val clearSignal: System.Signals.signal -> unit
   nonfix it selectCurrent selectBackTrace selectNear
       selectNext selectPrev showEvents breakWhen breakNear
       deleteBreak clearBreaks showBreaks showBreakTimes
       toggleBreak bfunc tfunc nofunc current ss ssb forward backward
       jump sk skb jumpTrace showCalls upCall downCall
       showVal emacsShowVal complete abort usedbg usedbg_live
       usedbg_stream usedbg_string usedbg_script run execute debugdebug cd 
       haltOnSignal setSignal clearSignal
end
 = struct
 open System.Control.Debug
 open UserDebugUtil UserDebugInterface UserDebugBreaks UserDebugEmacs

 val say = System.Print.say

 type int = int
 type string = string
 type unit = unit
 type 'a option = 'a option
 type 'a ref = 'a ref
 nonfix it selectCurrent selectBackTrace selectNear
        selectNext selectPrev showEvents breakWhen breakNear
	deleteBreak clearBreaks showBreaks showBreakTimes
	toggleBreak bfunc tfunc nofunc current ss ssb forward backward
	jump sk skb jumpTrace showCalls upCall downCall
	showVal emacsShowVal complete abort usedbg usedbg_live
	usedbg_stream usedbg_string usedbg_script run execute debugdebug cd 
	haltOnSignal setSignal clearSignal


 val _ = debugging := true

 (* When running under Emacs, we need to know the current working 
  * directory so that we can equate filenames that the user gives us, 
  * which may be relative to the current directory, with filenames that
  * Emacs gives us, which are absolute.
  * getWD is horribly slow and unreliable, so we remember the current directory
  * in currentWD, rebind cd to update this reference, and force an emacs
  * dbg session to issue a cd to the current directory first thing. *)
 val currentWD = ref ""
 fun cd s = (System.Directory.cd s;
	     currentWD := absolute s (!currentWD))


 (* Selection functions *)

 fun selectCurrent () =
 (* Command run by M-c in emacs.  Select the current event. *)
     emacsSelect (!currentEv)

 fun selectBackTrace () =
 (* Select the backtrace event, or the current event if there is no
  * backtrace event.  Not currently used; perhaps this should be bound
  * to some key? *)
     emacsSelect (case !backtrace of
		      SOME (_, (ev, _)) => SOME ev
		    | NONE => !currentEv)

 fun selectNear filename cp =
 (* Command run by M-e in emacs.  Select an event near the given character
  * position. *)
     emacsSelect
     (SOME (hd (ZeventsAfterLocation
		(if filename = runBufferName
		     then runName else filename, cp)))
      handle Hd => NONE)

 fun selectNext () =
 (* Command run by M-n in emacs.  Select the next event. *)
     case ofSome(nextEvent, !selected) of
	 SOME e => emacsSelect (SOME e)
       | NONE => emacsError "No further events"

 fun selectPrev () =
 (* Command run by M-p in emacs.  Select the previous event. *)
     case ofSome(prevEvent, !selected) of
	 SOME e => emacsSelect (SOME e)
       | NONE => emacsError "No previous events"

 (** Text display functions.
  ** These display debugger information in a textual format. *)

 fun prLoc ((file:filename,charno:charno),vis:visible) =
   (say "file \""; say file; say "\"";
    if not vis then say " [hidden version]" else ();
    if !emacs then 
      (say " char "; say(makestring charno))
    else 
      let val (line,pos) = ZlineposForCharno (file,charno)
      in say " line "; say(makestring line); say " pos "; print(makestring pos)
      end)

 fun prWhere where =
   let val (s,_,loc,vis) = ensureD(ZeventDesc where, "prWhere") 
   in say (s ^ " event at "); prLoc (loc,vis)
   end

 fun prWhereWhen ((where,when):wherewhen) =
     (prWhere where;
      printL [" (time ", makestring when, ")"])

 fun prExn () =
   let val exn = 
	   safeQuery(fn now => 
		     let val (_,finalTime) = YboundingTimes()
		     in case Yexception() of
			 SOME exn => if now = finalTime then
			     SOME exn
				     else NONE
		       | NONE => NONE
		     end)
   in case exn of 
        SOME exn => (say "[Execution blocked by exception: ";
	             say (System.exn_name exn);
		     say " ";
		     case exn of 
		       (* known exceptions *)
		       Io s => say s
		     | System.Unsafe.CInterface.SystemCall s => say s
		     | _ => (* unknown exn *)
			    safeQuery (fn _ =>
					 case YexnArg exn of
					   SOME (x,t) => YprintVal(x,t)
					 | NONE => ());
		     say "]\n")
      | NONE => ()
   end


 fun prSignal () =
  let val signal = safeQuery (fn _ => Ysignal())
      open System.Signals
      fun makestring SIGHUP = "SIGHUP"
        | makestring SIGINT = "SIGINT"
	| makestring SIGQUIT = "SIGQUIT"
	| makestring SIGALRM  = "SIGALRM"
	| makestring SIGTERM = "SIGTERM"
	| makestring SIGURG = "SIGURG"
	| makestring SIGCHLD = "SIGCHLD" 
	| makestring SIGIO = "SIGIO"
	| makestring SIGWINCH = "SIGWINCH"
	| makestring SIGUSR1 = "SIGUSR1"
	| makestring SIGUSR2 = "SIGUSR2"
	| makestring SIGTSTP = "SIGTSTP"
	| makestring SIGCONT = "SIGCONT"
	| makestring SIGGC = "SIGGC"
        | makestring SIGVTALRM = "SIGVTALRM"
	| makestring SIGPROF = "SIGPROF"
  in case signal of
       SOME signal => say (implode["[About to deliver signal ",
				     makestring signal, ".]\n"])
     | NONE => ()
  end

 val haltOnSignal = ZhaltOnSignal

 val setSignal = XsetSignal
 val clearSignal = XclearSignal

 fun showEvents filename =
    let fun f loc =
	  let val whrl = ZeventsAfterLocation loc
	      val (_,_,(_,charno),_) =
		  ensureD(ZeventDesc (hd whrl) (* may raise Hd *),
			  "showEvents")
	      fun p whr =
		let val (s,_,_,_) = ensureD(ZeventDesc whr, "showEvents")
		in say "\t"; say s
		end
	  in if !emacs then say(makestring charno)
	     else let val (line,pos) = ZlineposForCharno (filename,charno)
		  in say(makestring line); say "\t"; say(makestring pos)
		  end;
	     app p whrl;
	     say "\n";
	     f (filename,charno+1)
	  end
    in f (filename,1) handle Hd => ()
    end

 (** Breakpoints
  ** Every breakpoint has an associated id, which is returned when the
  ** breakpoint is set and which is used to refer to the breakpoint. *)

 (* Insert a breakpoint at a given time. Intended for use outside of emacs. *)
 fun breakWhen time =
     insertBreak (TIME time)

 (* Set breakpoint at an event near given line/character position. 
    Intended for use outside of emacs *)
 fun breakNear (filename:string,line:int,pos:int) : int option =
     let val cp = ZcharnoForLinepos(filename,line,pos)
     in SOME (insertBreak (EVENT (hd (ZeventsAfterLocation (filename,cp)))))
	 handle Hd => NONE
     end

 (* For internal use. *)
 fun breakWhere place = 
 (* Insert a breakpoint at the given event. *)
     emacsModify [place] (fn () => insertBreak (EVENT place))

 fun modifyBreak bn f =
 (* Perform a function that may modify the given breakpoint, updating the
  * screen appropriately if under emacs. *)
   if !emacs then
     let val ev =
	 case (getBreak bn) of
	     SOME (EVENT e) => SOME e
	   | _ => NONE
     in emacsModify (somes [ev]) f
     end
   else f()


 fun deleteBreak (bn:int) =
 (* Delete a breakpoint, given its id. *)
     modifyBreak bn
      (fn () =>
       (resetBreakFunc bn;
	if (not (removeBreak bn)) then
	  (say "[Error: breakpoint #"; say(makestring bn); say " doesn't exist.]\n")
	else ()))

 fun clearBreaks () =
 (* Delete all breakpoints. *)
     app (fn (n, _) => deleteBreak n) (!breakList)

 fun setBreakFunc (bn, f) =
 (* Set the break function at the given breakpoint. *)
     modifyBreak bn (fn () =>
		     (resetBreakFunc bn;  (* in case some function was already
					   * there *)
		      breakFuncList := (bn, f) :: (!breakFuncList)))

 fun showBreaks () =
 (* Show all breakpoints.  Primarily intended to be used outside emacs, but
  * might be useful in Emacs as well. *)
   let fun p (n,TIME whn) =
		 (say(makestring n); say "\t"; say "Time ";
		  say(makestring whn); say "\n")
	 | p (n,EVENT place) =
		 (say(makestring n); say "\t"; prWhere place; say "\n")
   in say "Breakpoints:\n";
      app p (!breakList)
   end

 fun showBreakTimes () =
 (* Show break times in the Emacs minibuffer.  Invoked by C-M-k. *)
     let val btimes =
	 (fold
	  (fn ((_, TIME t), s) => s ^ " " ^ makestring t
		      | (_, s) => s) (!breakList) "")
     in
     emacsMessage
     ("Time breakpoints:" ^
      (if (String.length btimes > 0) then btimes else " (none)"))
     end

 fun toggleBreak () =
 (* Command run by M-k in Emacs.  Toggle whether there is a breakpoint at
  * the selected event. *)
     case !selected of
	 NONE => ()
       | SOME ev => 
	     (case breakId ev of
		  SOME n => deleteBreak n
		| NONE => breakWhere ev)

 fun currentBreak () = (* currently unused *)
 (* Returns the breakpoint number of the breakpoint at the current event
  * or at the current time. *)
     let val eventB = ofSome(breakId, SOME(establishedPlace()))
	 val eventT = ofSome(breakIdAtTime, SOME(establishedTime()))
     in if isSome eventB then eventB else eventT
     end

 fun selectedBreak () = (* currently unused *)
 (* Returns the breakpoint number of the currently selected breakpoint,
  * if any. *)
     ofSome(breakId, !selected)

 fun doBreakFunc () =
 (* Perform the break function at the current event or time.
  * Executed after we have stopped at a breakpoint. *)
     case ofSome(getBreakFunc, currentBreak ()) of
	 SOME f => f ()
       | NONE => ()

 fun bfunc f =
 (* Sets f to be the break function at the currently selected breakpoint.
  * User function. *)
     case selectedBreak () of
	 SOME bn => setBreakFunc (bn, f)
       | NONE => say "[No breakpoint is selected]\n"

 fun tfunc t f =
 (* Sets f to be the break function at the breakpoint at the given time.
  * User function. *)
     case breakIdAtTime t of
	 SOME bk => setBreakFunc (bk, f)
       | NONE => printL ["[No breakpoint exists at time ", makestring t, "]\n"]

 fun nofunc t =
 (* Resets the break function at the breakpoint at the given time.
  * User function. *)
     case breakIdAtTime t of
	 SOME bk => resetBreakFunc bk
       | NONE => printL ["[No breakpoint exists at time ", makestring t, "]\n"]

 (** Execution-related commands *)
 val envTime : time ref = ref 0
 fun setEnvTime () =
    let val time = 
           case onSome(#2,!backtrace) of
	     NONE => safeQuery(fn t => t)
	   | SOME (_,t) => t
    in envTime := time;
       ZsetEnvTime time
    end

 fun prCurrent (s:string) =
 (* For use outside Emacs.  Display the current event in a textual form. *)
    let fun f ww = (say "[";say s; say " "; prWhereWhen ww; say "]\n";
		    prExn(); prSignal())
    in if ZinDebug() then
	   f (safeQuery (fn t => (hd(YcurrentPlaces()),t)))
       else printNotUnder()
    end

 fun current() = prCurrent "At" 

 fun doMove f =  (* both *)
 (* Execute the given function, which causes the current code position to
  * change, while maintaining the display and its state variables. 
  * ZinDebug is assumed true. *)
     if !emacs then
	 (emacsDeselect ();
	  (case f () of
	     COMPLETED _ => (prExn(); prSignal())
	   | INTERRUPTED _ => (prExn(); prSignal(); emacsError "(Interrupted)")
	   | NOTRUNNING => raise (DebugUserError "domove"));
	  setEnvTime();
	  emacsUpdate ())
     else (selected := NONE; 
	   backtrace := NONE; 
	   currentEv := NONE;
	   case f () of
	     COMPLETED _ => prCurrent "Stopped at"
	   | INTERRUPTED _ => prCurrent "Interrupted at"
	   | NOTRUNNING => raise (DebugUserError "domove");
	   setEnvTime())

 fun moveUnderDebug f =  (* both *)
   if ZinDebug() then doMove f else printNotUnder()

 fun step () =
 (* Attempt step forward.  Does not update display. *)
   Xjump (establishedTime() + 1)

 fun stepb () =
 (* Attempt step backward. Does not update display. *)
   Xjump (establishedTime() - 1)

 fun ss () = moveUnderDebug step

 fun ssb () =  moveUnderDebug stepb

 fun goforward () =  
 (* Move forward until breakpoint or end of compilation unit.
    Do break function if any. *)
     let val now = establishedTime()
	 val (_,finalTime) = safeQuery(fn _ => YboundingTimes ())
	 val minbtime = fold (fn ((_,TIME t),m) => if t > now
						then min(t,m)
						else m
				| (_,m) => m) 
			       (!breakList) finalTime
         fun lastTime p = 
	    if exists (fn p' => p' = p) (YcurrentPlaces()) then
	      YcurrentTime()
	    else #1(YlastTimes p)
	 fun getetimes () = fold (fn ((_,EVENT p),etl) => 
					  lastTime p :: etl
				    | (_,etl) => etl) (!breakList) []
         val etimes = safeQuery (fn _ => getetimes())
	 fun minchanged () = 
	    fold (fn ((old,new),m) =>
		       if (new > old) andalso (new > now) andalso 
			  (new < m) then new else m)
		 (pairlist etimes (getetimes())) Zinfinity
     in XbinSearch(minchanged,minbtime,false)
     end before
     doBreakFunc()

 fun forward () =  moveUnderDebug goforward

 fun backward() =
 (* Move backward until breakpoint or start of compilation unit.
    Do break function if any. *)
    moveUnderDebug (fn () =>
      let val now = establishedTime()
          val (initialTime,_) = safeQuery (fn _ => YboundingTimes())
	  val target = safeQuery (fn _ =>
		 fold (fn ((_,TIME t), m) => if (t < now) then max(t,m) else m
	  	        | ((_,EVENT p), m) => max (#2(YlastTimes p),m))
		      (!breakList) initialTime)
      in Xjump target
      end before 
      doBreakFunc())

 fun jump t =
     moveUnderDebug (fn () => Xjump t)

 fun skip () =
 (* Skip forward, using binary search primitive.
  * Does not update display. *)
      let val now = establishedTime()
	  fun check_ancestors () = 
	    (* return upper bound on time when time 'now' is no longer on stack
	       or infinity if no such bound known. 
	       Note: this function can take a painfully long time to 
	       evaluate; on interrupt, Ycaller should return 0, terminating
	       the recursion in f... *)
	    let fun parent t = let val (_,(_,pt)) = Ycaller t
			       in pt end
		fun f (0,bound) = bound (*??*)
		  | f (when,bound) = 
		       if when = now then Zinfinity
		       else if when < now then bound
		       else f (parent when,when)
		val ct = YcurrentTime()
	    in f (ct,ct)
	    end			    
       in if (safeQuery YatCall) then
	     XbinSearch(check_ancestors,Zinfinity,true)
          else
	    step()
       end

 fun sk() =
   moveUnderDebug skip

 fun skipb () =
 (* Skip backward.
  * Does not update display. *)
    let val now = establishedTime()
	fun parent t = let val (_,(_,pt)) = Ycaller t in pt end
	val init_parent = parent now
	fun f(0,s) = s (* paranoia *)
	  | f(t,s) = 
	    if t = init_parent then
	      s
	    else f(parent t,SOME t)
    in case Xjump(now-1) of
	 COMPLETED (w,t) => 
	  (case XwithEstablishedTime(fn _ => f(t,NONE)) of
	     COMPLETED(NONE) => COMPLETED (w,t)
	   | COMPLETED(SOME t) => Xjump t
	   | INTERRUPTED _  => INTERRUPTED (w,t)
	   | NOTRUNNING => NOTRUNNING)
       | INTERRUPTED ww => INTERRUPTED ww
       | NOTRUNNING => NOTRUNNING
    end

 fun skb() = moveUnderDebug skipb

 fun jumpTrace () =
 (* If the backtrace event is selected, jump to its time.
  * Invoked by M-t in emacs. *)
     case !selected of
	 NONE => ()
       | SOME ev =>
	     case onSome(#2, !backtrace) of
		 SOME (place,time) =>
		     if ev = place then jump time else ()
	       | NONE => ()


 (** Stack backtrace commands *)

 fun showCalls maxdepth =  
   let fun p (top::rest) =
	    let fun prvar ((n:string,t:ty),v:value) = 
		     (say "\t"; say n; say " = "; YprintVal(v,t))
		fun prcall (w as (whr,whn),vw as (vwhr,vwhn),bvlist) = 
		   (prWhereWhen w; say "\n";
		    if (whn > 0) then
		      (if (vwhn < whn andalso vwhn > 0) then
			 (say "via\t"; prWhereWhen vw; say "\n")
		       else ();
		       say "  bound values:"; app prvar bvlist; say "\n";
		       if (vwhn > 0) then
			 (say "  call: "; YprintBind (vw,8); say "\n")
		       else ())
		    else ())
	    in say "At\t"; prcall top;
	       app (fn c => (say "From\t"; prcall c)) rest
	    end
	 | p _ = ()
       fun f t = p (YcallTrace (max(maxdepth,1)-1) t)
   in interruptableQuery f
   end

local 
 fun backtr () =  onSome(#1,!backtrace)
in
 fun upCall () =  (* both *)
    if ZinDebug() then
       ((case backtr() of
 	  NONE => emacsSetBackTrace (SOME 1)
	 | SOME n => emacsSetBackTrace (SOME (n+1)))
		    handle SetBackTrace => emacsError "At top of call chain";
       setEnvTime())
    else printNotUnder()

 fun downCall () = (* both *)
    if ZinDebug() then
      (case backtr() of
  	  NONE => emacsError "At bottom of call chain"
        | SOME 1 => emacsSetBackTrace NONE
        | SOME n => emacsSetBackTrace (SOME (n-1));
       setEnvTime())
    else printNotUnder()
end

 (** Variable display functions *)
 fun showVal n =  
 (* Print a value and the position of its binding site.
  * For use outside Emacs. *)
   let fun f _ = 
     case YgetVal n (!envTime) of
       SOME(v,t,w as (whr,whn)) =>
	 if ZisFn(t)
	 then (say n; say "\tfunction bound by code:\n\t\t";
	       YprintBind(w,16); say "\n";
	       say "\t\t["; prWhereWhen w; say "]\n")
	 else (say n; say "\t"; YprintVal(v,t); 
	       say "\t["; prWhereWhen w; say "]\n")
     | NONE => (say n; say "\tNot bound\n")
   in interruptableQuery f
   end

 fun emacsShowVal n = 
 (* Like the preceding function, but moves the selection to the binding site
  * of the variable or function.
  * Invoked by M-l in Emacs. *)
   let fun f _ = 
       case YgetVal n (!envTime) of
	 SOME(v, t, w as (whr, _)) =>
	      (printL [n, " = "];
	       YprintVal(v,t);
	       say "\n";
	       emacsSelect (SOME whr))
       | NONE => printL [n, " is not bound\n"]
   in interruptableQuery f
   end

 (** Miscellaneous functions *)

 fun debugdebug (on:bool) = Wdd := on
   (* call with true for internal debugger diagnostics *)

 fun startUp s () =  (* interactive startup *)
    (if !emacs then
       emacsInitDebug s
     else ();
     setEnvTime())

 fun nullStartUp () = ()  

 fun shutDown () = (* interactive shutdown *)
    (if !emacs then
       (clearBreaks();  (* remove all breakpoints *)
	emacsTermDebug())
     else (breakList := nil;
	   selected := NONE;
	   backtrace := NONE;
	   currentEv := NONE))

 fun complete() = 
 (* Complete the execution of the compilation unit. *)
     if ZinDebug() then
	 (shutDown();
	  Xcomplete(); 
	  ()) (* doesn't return if successful *)
     else printNotUnder()

 fun abort() = 
 (* Abort execution of compilation unit. *)
     if ZinDebug() then
	 (shutDown();
	  Xabort();
	  ())    (* doesn't return if successful *)
     else printNotUnder()

 (** Source code functions *)

 fun usedbg file = 
     if ZinDebug() then
       say "[Already running under debugger.]\n"
     else 
       (* If under emacs we convert the filename to absolute form 
	  because Emacs will send that form of filename to us. *)
       let val file = if !emacs then absolute file (!currentWD) else file
       in Xuse_file(FULL,file)
       end

 fun usedbg_live file = 
     if ZinDebug() then
       say "[Already running under debugger.]\n"
     else 
       let val file = if !emacs then absolute file (!currentWD) else file
       in Xuse_file(LIVE (NONE,startUp NONE,shutDown), file)
       end

 fun usedbg_stream s = 
     if ZinDebug() then
       say "[Already running under debugger.]\n"
     else Xuse_stream(FULL,s)

 fun usedbg_string s = usedbg_stream (open_string s)

 fun usedbg_script (source:instream) (commands:instream) =
    if ZinDebug() then
      say "[Already running under debugger.]\n"
    else 
      Xuse_stream(LIVE(SOME ("<script>",commands),startUp NONE,shutDown),
		  source)

 fun run s =
    if ZinDebug() then
      say "[Already running under debugger.]\n"
    else 
      Xuse_stream(LIVE(NONE,startUp (SOME s),shutDown),open_string s)

 fun execute s =
    if ZinDebug() then    
      if safeQuery(fn now => 
	     let val (_,finalTime) = YboundingTimes()
	     in now < finalTime
             end) then 
	  (say "[processing interpolation]\n";
	   Xinterpolate_stream(open_string s);
	   jump Zinfinity (* should stop at end of interpolation *))
      else say "[Cannot interpolate at end of compilation unit.]\n"
   else printNotUnder()
end






