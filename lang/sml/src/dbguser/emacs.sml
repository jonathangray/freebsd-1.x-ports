(* Emacs Display functions. *)

 (** Display functions.  sml can be run inside emacs using a sml mode written
  ** in Emacs Lisp (see sml-mode.el, sml-debug.el).
  ** All display updates are performed through these functions. *)

structure UserDebugEmacs =
struct
 local
 open UserDebugUtil UserDebugInterface UserDebugBreaks
 in
 val emacs = ref false     (* Are we running sml in an Emacs window? *)

 fun emacsInit () = (* Emacs sends a "emacsInit()" when an sml window starts *)
     (emacs := true;
      set_term_in (IO.std_in, true);
      set_term_out (IO.std_out, true))

 (* currentEv is the event that is displayed as the current event.
  * Each time that execution stops, currentEv is set to the value
  * returned by currentPlace().*)

 val currentEv = ref (NONE : place option)

 (* The selected event is an event which the user uses as a cursor
  * to browse through and choose events. *)

 val selected = ref (NONE : place option)

 (* The backtrace event is the event which the user has chosen by moving
  * up and down in the calling stack.  We store an index into the "stack"
  * as well as the (where, when) of the trace event, since Ycaller is 
  * expensive. *)
 val backtrace = ref (NONE : (int * wherewhen) option)  

 fun emacsMessage message =
     printL ["(emacs (message \"", message, "\"))\n"]

 (* Filename for events in run text is "<instream>". *)
 val runName = "<instream>"
 val runBufferName = "*smld-run*"

 (* Filename for events in interpolations is "<interpolation>". *)  
(*  val executeName = "<interpolation>"
    val executeBufferName = "*smld-execute*" *)

 fun emacsCreateBuffer name contents =
     printL ["(emacs (sml-create-buffer \"", name, "\" \"", contents, "\"))\n"]

 fun emacsKillBuffer bufName =
     printL ["(emacs (sml-kill-buffer \"", bufName, "\"))\n"]

 fun emacsError s =
     printL ["(emacs (sml-error \"", s, "\"))\n"]

 fun emacsLabelCommand display file (pos:int) string cursor =
 (* Display (display = true) or undisplay (display = false) a label in an
  * Emacs buffer.  For displaying, cursor should have the value SOME b,
  * where b represents whether we should move the cursor to the beginning
  * of the label. *)
     printL ["(emacs (sml-",
	     if display then "label" else "unlabel",
	     "-buffer \"",
	     if file = runName then 
	       runBufferName 
(*	     else if file = executeName then
	       executeBufferName *)
   	     else file,
	     "\" ", makestring pos,
	     " \"", string, "\"",
	     case cursor of SOME b => if b then " t" else " nil" | NONE => "",
	     "))\n"]


 fun emacsGoodBye () =
     printL ["(emacs (sml-good-bye))\n"]

 fun emacsEvent display ev =
 (* Display (display = true) or undisplay (display = false) a given event.
  * An event is only displayed if
      - it is the selected event
      - it is the current event
      - it is the backtrace event
      - there is a breakpoint at that event *)
     let val isSelected = eqOption (ev, !selected)
	 val isCurrent = eqOption (ev, !currentEv)
	 val isBacktrace =
	       case !backtrace of
		 SOME (i, (where, when)) =>
		     if ev = where then
		       SOME when
		     else NONE
	       | NONE => NONE
	 val isBreakpoint = isSome (breakId ev)
	 val displayed = isSelected orelse isCurrent orelse
			 isSome(isBacktrace) orelse isBreakpoint
     in
	 if displayed then
	     let val s =
		 implode [if isSelected then "[" else "<",
			  case breakId ev of
			      NONE => ""
			    | SOME bn =>
				  if isSome (getBreakFunc bn)
				      then "bk*:"
				      else "bk:",
			  eventText ev,
			  if isCurrent then
			      ":" ^ (makestring (establishedTime()))
			      else "",
			  case isBacktrace of
			      SOME when => ":bt:" ^ (makestring when)
			    | NONE => "",
			  if isSelected then "]" else ">"]
	     in case eventLocation ev of
		  SOME (file,pos) =>
	                 emacsLabelCommand display file pos s
			      (if display then SOME isSelected else NONE)
	        | NONE => if display then 
		            emacsError ("No source available for " ^ s)
			  else ()
	     end
	 else ()
     end

 val emacsDisplay = emacsEvent true
 val emacsUndisplay = emacsEvent false

 fun emacsModify evl f =
 (* Perform a function that may modify the appearance of the given events. *)
     let val evl' = uniq evl in	(* don't display or undisplay an event twice *)
	 (app emacsUndisplay evl';
	  f ();
	  app emacsDisplay evl')
     end

 fun emacsSelect ev =
 (* Select the given event and update the display.
  * The selected event is the one on which the event cursor rests. *)
     emacsModify (somes [!selected, ev]) (fn () => selected := ev)

 exception SetBackTrace
 fun emacsSetBackTrace (bt:int option) = 
 (* Set the backtrace event to that indexed by the given integer in the
  * call trace list, or to NONE; selects the backtrace event, or the
  * current event if there is no backtrace event.
  * Updates the display. *)
     let val cww = safeQuery(fn t => (hd(YcurrentPlaces()),t))
         val new = case bt of
	     SOME n =>
	       (case (case !backtrace of 
			SOME (n',ww') => 
			  if n' < n then 
			    traceEvent ww' (n-n')
			  else traceEvent cww n 
		      | NONE => traceEvent cww n) of
		  SOME ww => SOME (n, ww)
		| NONE => raise SetBackTrace)
	   | NONE => NONE
	 val oldEv = onSome(#1 o #2, !backtrace)
	   (* event number of old backtrace event *)
	 val newEv = onSome(#1 o #2, new)
	 val newSel = case newEv of
		       SOME e => SOME e
		     | NONE => !currentEv
     in
	 emacsModify (somes [oldEv, newEv, !selected, newSel])
	   (fn () => (backtrace := new;
		      selected := newSel))
     end

 fun emacsSetCurrent ev =
 (* Set the current event, updating the display appropriately. *)
     emacsModify (somes [!currentEv, ev]) (fn () => currentEv := ev)

 (* The functions emacsDeselect and emacsUpdate could be expressed using
  * the three functions above, but call emacsModify themselves to optimize
  * the number of emacs commands that are generated. *)

 fun emacsDeselect () =
 (* Invoked each time before execution begins.
  * Reset the selected, backtrace and current events. *)
     emacsModify (somes [!selected, onSome(#1 o #2, !backtrace),
			 !currentEv]) (fn () =>
				       (selected := NONE;
					backtrace := NONE;
					currentEv := NONE))

 fun emacsUpdate () =
 (* Invoked each time that execution stops, to set the current and selected
  * events. *)
	 let val where = SOME(establishedPlace()) in
	     emacsModify (somes [!currentEv, !selected, where])
	     (fn () => (currentEv := where; selected := where))
	 end

 local 
   val runBuffer = ref false
 in 
   fun emacsInitDebug sopt =
     ((* emacsCreateBuffer executeBufferName ""; *)
      case sopt of
	SOME s => (emacsCreateBuffer runBufferName s; 
		   runBuffer := true)
      | NONE => runBuffer := false;
      emacsUpdate())

  fun emacsTermDebug () = 
    (emacsDeselect();
     if (!runBuffer) then
       emacsKillBuffer runBufferName
     else() (* ;
     emacsKillBuffer executeBufferName *))
 end 

 end (* local *)

end (* structure *)
