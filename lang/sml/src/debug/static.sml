(* DebugStatic

   Handles (mostly) compile-time data about debuggable code.  

   Events are defined as tagged pointers into abstract syntax. Tags (expressed
     as the constructors of datatype event) indicate the variety of event.
     There are operators to return standard text description of the event
     variety, the number of arguments associated with the event, and the
     location (character position) of the event within the source file
     from which it was compiled. Each event has a unique identifying 
     integer (place).

   The instrumenter coalesces neighboring events (roughly when they are
     in the same basic block) into *clusters*; the clock is only
     incremented at clusters, and control can be returned to the 
     debugger only at the final event of a cluster. Each cluster
     has an associated identifying number (evn) and a vector of event
     places, in reverse order of their source sequence.  
     The event at the head of this vector appears in exactly one cluster; 
     other events in the vector may appear in more than one cluster. 

   This module maintains a global description of all events, indexed 
     by place, and of all clusters, indexed by evn.  For clusters, the 
     description includes the arrays of lastEvnTimes, which tell the last
     time each cluster was encountered during execution.  These arrays are
     maintained by special code added by the instrumenter, and are used by
     the breakpointing mechanism (see DebugMotions). (These arrays are
     actually part of the dynamic state, and thus constitute a
     history-maintaining subsystem.)  There is also a map evnsForEvent
     taking each event number to the set of cluster numbers containing it.

   There is also an index to all events keyed by location, i.e., filename
     plus character position. This is primarily intended as a means for
     setting breakpoints.  Multiple events can have the same character
     position; in this case their relative order is undefined.

   Events are added to these databases on a per-compilation-unit basis,
     immediately after instrumentation. Since there is no point in
     keeping event data if the compilation unit eventually terminates
     abnormally, we add the data only tentatively, and can later roll
     back the addition.  Each compilation unit occupies a contiguous
     range of place and evn numbers; these ranges begin with the same integer,
     but the place range is ordinarily bigger.  There is ordinarily no
     connection between places and evn's bearing the same number.

   Compilation units (and the events within them) have a visibility 
     attribute: this is set false (by an appropriate call from the
     interactive system) if the filename associated with the unit
     is recompiled, so that clients who use location data won't get confused
     about which file is really meant.
     A more elaborate system might user version numbers or datestamps to
     distinguish versions of the same file name.

   There are also a few *pseudo-events*. These are events that occur as a
     explicit debugger code (e.g., in the debugger's IO library) rather
     than within user code, and so have no location.  Each represents a
     distinct cluster. They are given fixed places, evn's and various 
     support functions.
*)

signature DEBUG_STATIC =
sig
  datatype event				(* objects passed at runtime *)
   = APPev of Absyn.exp
   | RAISEev of Absyn.exp
   | HANDLEev of Absyn.rule			(* bound value objs *)
   | FNev of Absyn.rule				(* bound value objs *)
   | CASEev of Absyn.exp * Absyn.rule		(* bound value objs *)
   | VALev of Absyn.dec				(* bound value objs *)
   | VALRECev of Absyn.dec			(* bound value objs *)
   | STRev of Absyn.dec                         (* time array, strvar objs *)
   | ABSev of Absyn.dec   	 	        (* time array, strvar objs *)
   | FCTev of Absyn.dec                         (* fctvar objs *)
   | SIGev of Absyn.dec
   | FSIGev of Absyn.dec
   | TYPEev of Absyn.dec
   | FIXev of Absyn.dec
   | OVLDev of Absyn.dec
   | EXCEPTIONev of Absyn.dec			(* exception objs *)
   | FCTENTev of Absyn.fctb
   | FCTAPPev of Absyn.strexp                   (* param strvar obj *)
   | STRENDev of Absyn.strexp
   | STRVARev of Absyn.strexp
   | OPENev of Absyn.dec
   | LETev of Absyn.exp
   | LOCALev of Absyn.dec
   | LOCINev of Absyn.dec
   | LOCENDev of Absyn.dec
   | IOev     (* pseudo-event at IO, etc. *)
   | UNCAUGHTev  (* pseudo-event at uncaught exception *)
   | SIGSTARTev (* pseudo-event at beginning of signal handler *)
   | SIGENDev (* pseudo-event at end of signal handler *)  
                                                 (* matching SIGSTART time *)
   | STARTev of Absyn.dec (* entire compilation unit *)
   | ENDev of Absyn.dec (* entire compilation unit *) (* matching START time *)
   | NULLev

  type time (* = int *)

  type filename (* = string *)
  type charno (* = int *)
  type location (* = filename * charno *)
  type visible (* = bool *)

  type place (* = int *)
  type evn (* = int *)
  type evindex (* = int *)
      (* Index into vector of events for an evn *)

  (* Displayable text corresponding to variety of event. *)
  val eventText: event -> string
  (* Number of arguments passed to break with this event. *)
  val argCnt: event -> int
  (* Location of event in source code; filename is factored out. *)
  val locOfEvent:event -> charno
  
  (* Utility functions for pseudo-events. *)
  (* Return pseudo-evn corresponding to event. *)
  val pseudoEvn: event -> evn
  (* Tell if event is pseudo. *)
  val isPseudo: event -> bool
 
  (* Compilation unit handling. *)
  val install: {inputSource:Source.inputSource,
		firstPlace:place,events: event Vector.vector, 
		evns: place Vector.vector Vector.vector} -> unit
      (* Add the described events (produced by instrumenting the file)
         to our databases, but only tentatively. *)
  val rollback: unit -> unit (* remove tentative installations *)
  val nextPlace: unit -> place  (* next free place *)

  (* This function should be called by the interactive system once
     per *file* (not comp unit) before installing any new events.
     It marks any previous comp units tagged with this file name as hidden. *)
  val hideFile:filename -> unit 

  (* Following operate on all units, including tentatively installed ones. *)
  val evnTimesArray: evn -> time array
     (* Return array that begins with this evn.  Used only to pass the array
        to instrumented code at runtime. *)
  val lastTime: evn -> time  (* Returns evnTimesArray entry for evn *)
  val placesFor : evn -> place list (* Return places for evn *)
  val eventsFor: evn -> event list (* Returns events for evn *)
  val filenameFor: place -> (filename * visible)
      (* Returns filename and visibility attribute for place *)
  val eventPlacesAfter: location -> place list
      (* Returns places at or after location *)
  val eventPlacesBefore: location -> place list
      (* Returns places at or before location *)
  val charnoForLinepos: filename * int * int -> charno
      (* Returns charno correpsonding to (line,posn) in filename. *)
  val lineposForCharno : location -> int * int
      (* Returns (line,posn) corresponding to charno in filename. *)

  val eventForPlace: place -> event (* Returns event corresponding to place.*)
  val immediatePlaces: place list -> place list
      (* Returns prefix of list within a single abstraction. *)
  val evnsFor: place -> evn list  
      (* Returns evns immediately containing place. *)

  (* Support history mechanism for evnTimes arrays *)
  val rememberEvnTimes: unit -> DebugKernel.doers

  val lastUnitTime: unit -> time
      (* Returned last event execution time for last installed unit
         (tentative or perm) *)

end

structure DebugStatic: DEBUG_STATIC =
struct
  open Vector Array List DebugUtil DebugKernel Access Absyn ErrorMsg Types
  infix 9 sub

  fun vapp (f:'a -> 'b) (v:'a vector) =
    let fun loop n = (f (Vector.sub(v,n)); loop(n+1))
	               handle Vector.Subscript => ()
    in loop 0 
    end

  fun listofvector (v:'a vector) : 'a list =
    let fun loop(n,app) = loop(n-1,Vector.sub(v,n)::app)
	                      handle Vector.Subscript => app
    in loop(Vector.length v - 1,nil)
    end

  datatype event
   = APPev of exp
   | RAISEev of exp
   | HANDLEev of rule
   | FNev of rule
   | CASEev of exp * rule
   | VALev of dec
   | VALRECev of dec
   | STRev of dec
   | ABSev of dec
   | FCTev of dec
   | SIGev of dec
   | FSIGev of dec
   | TYPEev of dec
   | FIXev of dec
   | OVLDev of dec
   | EXCEPTIONev of dec
   | FCTENTev of fctb
   | FCTAPPev of strexp
   | STRENDev of strexp
   | STRVARev of strexp
   | OPENev of dec
   | LETev of exp
   | LOCALev of dec
   | LOCINev of dec
   | LOCENDev of dec
   | IOev    (* pseudo-event raised at IO, etc. *)
   | UNCAUGHTev (* pseudo-event raised on uncaught exception *)
   | SIGSTARTev (* pseudo-event at beginning of signal handler *)
   | SIGENDev (* pseudo-event at end of signal handler *)  
   | STARTev of dec
   | ENDev of dec
   | NULLev
  
  fun eventText (evt:event) : string =
       case evt of
	 VALev(_) => "VAL"
       | VALRECev(_) => "VALREC"
       | FNev(_) => "FN"
       | CASEev(_) => "CASE"
       | APPev(_) => "APP"
       | RAISEev(_) => "RAISE"
       | HANDLEev(_) => "HANDLE"
       | STRev(_) => "STRUCTURE"
       | ABSev(_) => "ABSTRACTION"
       | FCTev(_) => "FUNCTOR"
       | SIGev(_) => "SIGNATURE"
       | FSIGev(_) => "FUNSIG"
       | TYPEev(_) => "TYPE"
       | FIXev(_) => "FIXITY"
       | OVLDev(_) => "OVERLOAD"
       | EXCEPTIONev(_) => "EXCEPTION"
       | FCTENTev(_) => "FUNCTOR ENTRY"
       | FCTAPPev(_) => "FUNCTOR APP"
       | STRENDev(_) => "STRUCTURE END"
       | STRVARev(_) => "STRUCTURE VAR"
       | OPENev(_) => "OPEN"
       | LETev(_) => "LET"
       | LOCALev(_) => "LOCAL"
       | LOCINev(_) => "LOCAL IN"
       | LOCENDev(_) => "LOCAL END"
       | IOev => "IO"
       | UNCAUGHTev => "UNCAUGHT EXCEPTION"
       | SIGSTARTev => "ENTER SIGNAL HANDLER"
       | SIGENDev => "EXIT SIGNAL HANDLER"
       | STARTev(_) => "START"
       | ENDev(_) => "END"
       | NULLev => "NULL"
	


  type filename = string
  type charno = int  (* counting from 1 *)
  type location = filename * charno
  type visible = bool (* true if file has not been hidden by reusing *)
 
  type place = int
  type evindex = int

  type cud = {inputSource: Source.inputSource,
	      visible:visible ref,
	      firstPlace:place, 
	      events: event vector, 
	      evnsForEvent: evn list array,
	      evns: place vector vector,
	      evnTimes: time array}
  (* Both events and evns in the cud begin at firstPlace. *)

  (* initial cud contains backstop NULLev and other pseudo-events *)
  val pseudoCnt = 5
  val initialCud = {inputSource=Source.newSource("",0,std_in,false,
						 ErrorMsg.defaultConsumer(),
						 NONE),
		    visible=ref false,
		    firstPlace=0,
		    events=vector[NULLev,IOev,UNCAUGHTev,SIGSTARTev,SIGENDev],
		    evns=Vector.tabulate(pseudoCnt,fn x => vector[x]),
		    evnsForEvent=arrayoflist[[0],[1],[2],[3],[4]],
		    evnTimes=array(pseudoCnt,0)}
  fun pseudoEvn NULLev = 0
    | pseudoEvn IOev = 1
    | pseudoEvn UNCAUGHTev = 2
    | pseudoEvn SIGSTARTev = 3
    | pseudoEvn SIGENDev = 4
    | pseudoEvn _ = debugPanic "Static.pseudoEvn"
  fun isPseudo NULLev = true
    | isPseudo IOev = true
    | isPseudo UNCAUGHTev = true
    | isPseudo SIGSTARTev = true
    | isPseudo SIGENDev = true
    | isPseudo _ = false

  (* These structures support use of cuds and locIndexes on a tentative
     basis, before we know whether the unit has been successfully executed
     or not. Value currentCud refers to a tentative unit being executed 
     currently; value lastCud refers  to the last committed unit. *)
  val lastCud = ref initialCud
  val currentCud = ref initialCud

  fun badCud (key:int,exn,s) = 
      debugPanic(implode["!bad Cud ",makestring key," ",
			 System.exn_name exn," ",s])

  (* cud structure *)
  structure CudSet = SortedSet (
	   struct
	     type t = cud
	     type k = evn
	     fun key ({firstPlace,...}:cud) : place = firstPlace
	     val lt = Integer.<
	   end)
  
  local 
    open CudSet
    val cuds = ref (insert(new(),initialCud)) 
  in
    fun resetCuds () = cuds := insert(new(),initialCud)
    
    fun addCud (cud as {firstPlace,...}:cud) =
	 ((cuds := delete (!cuds,firstPlace)) handle NotFound => ();
	  cuds := insert (!cuds,cud))	
    
    fun evnTimesArray evn = 
      #evnTimes (find (!cuds,evn))
	handle exn => badCud (evn,exn,"evnTimesArray")
    
    fun lastTime evn  =  (* fetches value from within array *)
      let val {firstPlace,evnTimes,...} = findp (!cuds, evn)
      in evnTimes sub (evn-firstPlace)
      end handle exn => badCud (evn,exn,"lastTime")
    
    fun eventForPlace place =
      let val {firstPlace,events,...} = findp(!cuds,place)
      in Vector.sub(events,place-firstPlace)
      end handle exn => badCud (place,exn,"eventForPlace")

    fun placesFor evn =
      let val {firstPlace,evns,events,...} = findp (!cuds,evn)
      in listofvector (Vector.sub (evns,evn-firstPlace))
      end handle exn => badCud (evn,exn,"placesFor")

    fun eventsFor evn = map eventForPlace (placesFor evn)

    fun filenameFor place =
      let val {inputSource={fileName,...},visible,...} = findp (!cuds,place)
      in (fileName,!visible)
      end handle exn => badCud (place,exn,"filenameFor")

    fun evnsFor place =
      let val {firstPlace,evnsForEvent,...} = findp(!cuds,place)
      in Array.sub(evnsForEvent,place-firstPlace)
      end handle exn => badCud (place,exn,"evnsFor")

    fun hideCud cud = #visible(cud:cud) := false

    local 
      structure EvnTimesSet = SortedSet (
	   struct
	     type t = evn * time array
	     type k = evn
	     fun key (t,_) : evn = t
	     val lt = Integer.<
	   end)

      fun saveEvnTimes() : EvnTimesSet.s =
        let fun f ({firstPlace,evnTimes,...}:cud,ets) =
	      EvnTimesSet.insert(ets,(firstPlace,ArrayExt.copy evnTimes))
	in fold (!cuds,f,EvnTimesSet.new())
	end
				      
      fun restoreEvnTimes (ets:EvnTimesSet.s) =
        let fun f ({firstPlace,evnTimes,...}:cud) =
	      let val (_,eta) = EvnTimesSet.find(ets,firstPlace)
              in ArrayExt.reset(evnTimes,eta)
	      end handle EvnTimesSet.NotFound => 
		   (* zero times *)
		   let fun g n = (Array.update(evnTimes,n,0); g (n+1))
		   in g 0 handle Subscript => ()
		   end
        in iterate (!cuds,f)
	end
	  
    in
    fun rememberEvnTimes () = 
      let val ets = saveEvnTimes()
          fun reset _ = restoreEvnTimes ets
      in {undo=reset,redo=reset}
      end

    end (* local *)

  end (* local open structure CudSet *)

 
  (* Return prefix of place list up through first FN or HANDLE. *)
  fun immediatePlaces' ((p::r): place list,(ev::er):event list) =
      (case ev of
	 FNev _ => [p]
       | HANDLEev _ => [p]
       | _ => p::(immediatePlaces' (r,er)))
    | immediatePlaces' (nil,nil) = nil
    | immediatePlaces' _ = debugPanic "Static.immediatePlaces'"

  fun immediatePlaces (pl: place list) = 
      immediatePlaces' (pl, map eventForPlace pl)

  (* Traverse the opr of an APPexp to find the sub-node that should
     carry the APPexp's mark. *)
  fun locateOprMark(CONSTRAINTexp(exp,_)) = locateOprMark exp
    | locateOprMark(SEQexp[exp]) = locateOprMark exp
    | locateOprMark(APPexp(_,arg)) = locateOprMark arg
    | locateOprMark(RECORDexp[_,(_,arg2)]) = locateOprMark arg2
    | locateOprMark exp = exp

  val locOfEvent' =
  (* Return a character position corresponding to a given event.
   * The event marker actually falls between characters (or between tokens,
   * since white space is insignificant); its position is immediately BEFORE
   * the returned character position.
   *
   * For each match below, an event of the matching type will appear at each
   * position marked <*> in the following comment. *)
  
     fn VALev(MARKdec(_,s,e)) => s
	      (* <*> val a = 7 *)
  
      | VALRECev(MARKdec(_,s,e)) => s
	      (* <*> val rec a = 7 *)
  
      | FNev(RULE(_,MARKexp(_,s,e))) => s
	      (* (fn a => <*> a + 1) *)              (* explicit fn *)
	      (* fun f a = <*> a + 1 *)              (* implicit fn *)
	      (* fun f a <*> b <*> c <*> = a + b + c *)
			  (* nested implicit fn's -- N.B. doesn't work*)
      | FNev(_) => debugPanic "Static.locOfEvent bad FNev marking"
  
      | CASEev(_,RULE(_,MARKexp(_,s,e))) => s
	      (* case a of 1 => <*> b | 2 => <*> c *)
      | CASEev(_) => debugPanic "Static.locOfEvent bad CASEev marking"
  
      | APPev(APPexp(opr,_)) => 
	  (case locateOprMark opr of
	     MARKexp(_,s,e) => e
           | _ => debugPanic "Static.locOfEvent bad APPev marking")
	      (* f <*> b *)	                 (* non-infixed application *)
	      (* infix add                         (* infixed application *)
	       * 3 add <*> 4 *)
  
      | RAISEev(MARKexp(RAISEexp(MARKexp(_,s,e),_),_,_)) => s
	      (* raise <*> Div *)
      | RAISEev(MARKexp(_,s,e)) => s
	      (* <*> raise Match [produced by completeMatch] *)

      | HANDLEev(RULE(_,MARKexp(_,s,e))) => s
	      (* handle Match => <*> raise Overflow *)
      | HANDLEev(_) => debugPanic "Static.locOfEvent bad HANDLEev marking"
  
      | STRev(MARKdec(_,s,e)) => s 
	      (* <*> structure a = struct val d = 7 end *)
  
      | ABSev(MARKdec(_,s,e)) => s 
	      (* <*> abstraction a: ABSA = struct val d = 7 end *)
  
      | FCTev(MARKdec(_,s,e)) => s 
	      (* <*> functor afunct (b:C) = struct end *)
  
      | SIGev(MARKdec(_,s,e)) => s
	      (* <*> signature S = sig end *)

      | FSIGev(MARKdec(_,s,e)) => s
	      (* <*> funsig F(Q:sig end) = sig end *)
  
      | TYPEev(MARKdec(_,s,e)) => s
	      (* <*> datatype t = A | B of int *)
  
      | FIXev(MARKdec(_,s,e)) => s
	      (* <*> infix 6 + - *)
  
      | OVLDev(MARKdec(_,s,e)) => s
	      (* <*> overload ~:('a->'a) as Integer.~ and Real.~ *)
  
      | EXCEPTIONev(MARKdec(_,s,e)) => s
	      (* <*> exception E  *)
  
      | FCTENTev(FCTB{def=FCTfct{def=MARKstr(_,s,e),...},...}) => s
	      (* functor afunct (b:C) = <*> struct end *)
  
      | FCTAPPev(MARKstr(APPstr{argexp=MARKstr(_,s,e),...},_,_)) => s
	      (* structure d = afunct<*>(b) *)
      | FCTAPPev(_) => debugPanic "Static.locOfEvent bad FCTAPPev marking"
  
      | STRENDev(MARKstr(_,s,e)) => e
	      (* structure a = struct val b = 7 <*> end *)
  
      | STRVARev(MARKstr(_,s,e)) => e
	      (* structure a = b <*> *)
  
      | OPENev(MARKdec(_,s,e)) => s 
	      (* <*> open System.Control.Runtime *)
  
      | LETev(MARKexp(LETexp(MARKdec(_,s,e),_),_,_)) => e 
	      (* let val a = 5 val b = 7 <*> in c end *)
      | LETev(_) => debugPanic "Static.locOfEvent bad LETev marking"
  
      | LOCALev(MARKdec(LOCALdec(MARKdec(_,s,e),_),_,_)) => s
	      (* local <*> val b = 7 in val c = b end *)
      | LOCALev(_) => debugPanic "Static.locOfEvent bad LOCALev marking"

      | LOCINev(MARKdec(LOCALdec(_,MARKdec(_,s,e)),_,_)) => s
	      (* local val b = 7 in <*> val c = b end *)
      | LOCINev(_) => debugPanic "Static.locOfEvent bad LOCINev marking"
  
      | LOCENDev(MARKdec(LOCALdec(_,MARKdec(_,s,e)),_,_)) => e
	      (* local val b = 7 in val c = b <*> end *)
      | LOCENDev(_) => debugPanic "Static.locOfEvent bad LOCENDev marking"

      | IOev => 0
      | UNCAUGHTev => 0
      | SIGSTARTev => 0
      | SIGENDev => 0
      | STARTev (MARKdec(_,s,e)) => s
      | STARTev (_) => 0
      | ENDev (MARKdec(_,s,e)) => e 
      | ENDev (_) => 0
      | NULLev => 0
      | _ => debugPanic "Static.locOfEvent bad event type"
  
  fun locOfEvent x = locOfEvent' x - 1  (* adjust for parser weirdness *)
  
  (* Exclude events appearing in completion rules from index.
     Such events are identified by having identical start and
     end locations.  See elaborate/elabutil.sml completeMatch. *)
  fun indexable evt : bool =
     let fun indexableExp(MARKexp(_,s,e)) = s <> e
	   | indexableExp _ = true
         fun indexableRule(RULE(_,exp)) = indexableExp exp
     in case evt of
	  FNev rule => indexableRule rule
	| CASEev (_,rule) => indexableRule rule
        | HANDLEev rule => indexableRule rule
	| RAISEev exp => indexableExp exp
        | APPev(APPexp(opr,_)) => indexableExp (locateOprMark opr)
        | LETev exp => indexableExp exp
	| _ => true
     end			      
  
  fun dumpEvns (firstPlace:int, evns:place vector vector,events:event vector) =
    let fun pr_one place = 
	  let val evt = Vector.sub(events,place-firstPlace)
	      val charno = locOfEvent evt
	  in print (implode["\t",eventText evt," (",makestring place,")\t",
			    makestring charno,"\n"])
	  end
	fun pr n = let val places = Vector.sub(evns,n)
		   in print (firstPlace+n); 
		      vapp pr_one places; 
		      pr (n+1)
		   end  handle Subscript => ()
    in print "Evns:\n";    
       pr 0
    end		 
  
  fun dumpEvents (firstPlace:int, events: event vector, 
		  evnsForEvent:evn list array) =
    let fun pr n = 
	    let val evnstring = implode (map (fn evn => makestring evn ^ " ") 
					 (evnsForEvent sub n))
		val evt = Vector.sub(events,n)
	    in print(implode[makestring(firstPlace+n),
			     "\t",eventText evt," ( ",evnstring,")\t",
			     makestring (locOfEvent evt),"\n"]);
	       pr (n+1)			 
	    end handle Subscript => ()
    in print "Events:\n";    
       pr 0
    end		 


  (* LocIndex handling *)
  structure CharnoSet = SortedSet (
  struct
    type t = charno * (place list)
    type k = charno
    fun key (charno,wl) = charno
    val lt = Integer.<
  end)
  
  type locindex = (Source.inputSource * CharnoSet.s * (cud list)) list
  
  val emptyLocIndex = nil:locindex
  (* Following are similar to lastCud, currentCud, above. *)
  val lastLocIndex = ref emptyLocIndex
  val currentLocIndex = ref emptyLocIndex

  fun augmentLocIndex (oldIndex:locindex, 
		       cud as {inputSource=inputSource as {fileName,...},
			       firstPlace,events,...}:cud) : locindex =
    let fun putin (cs,evt,place) =
	  let open CharnoSet
	      val charno = locOfEvent evt
	  in if indexable evt andalso charno > 0 then
	       let val (_,placel) = find(cs,charno)
	       in update (cs, (charno,place::placel))
	       end handle NotFound => insert (cs, (charno,[place]))
	     else cs
	  end
	fun augcs cs =
	  let fun d (pl,cs) = 
	        let val evt = Vector.sub(events,pl)
		in d(pl+1, putin(cs,evt,firstPlace+pl))
		end handle Vector.Subscript => cs
	  in d(0,cs)
	  end
	fun augf ((f as (inputSource',cs,cuds))::rest) =
	      if fileName = #fileName inputSource' then 
		(inputSource',augcs cs,cud::cuds)::rest
	      else f::(augf rest)
	  | augf nil = [(inputSource,augcs (CharnoSet.new()),[cud])]
    in augf oldIndex
    end
  
  fun eventPlacesAfter (file:filename, charno:charno) : place list =
    let fun find ((inputSource: Source.inputSource,cs,_)::rest) = 
	      if file = #fileName inputSource then cs else find rest
	  | find nil = raise CharnoSet.NotFound
	val cs = find (!currentLocIndex)
	val (_,placel) = CharnoSet.finds (cs,charno)
    in rev placel
    end handle CharnoSet.NotFound => nil
  
  fun eventPlacesBefore (file:filename, charno:charno) : place list =
    let fun find ((inputSource:Source.inputSource,cs,_)::rest) = 
	      if file = #fileName inputSource then cs else find rest
	  | find nil = raise CharnoSet.NotFound
	val cs = find (!currentLocIndex)
	val (_,placel) = CharnoSet.findp (cs,charno)
    in rev placel
    end handle CharnoSet.NotFound => nil

  fun charnoForLinepos (file:filename, line:int, pos: int) : charno =
    let	fun find (({fileName,linePos,lineNum,...}:Source.inputSource,_,_)::rest) =
	      if file = fileName then 
		nth(!linePos,!lineNum - line) + pos
	      else find rest
	  | find nil = ~1
    in find (!currentLocIndex)
    end

  fun lineposForCharno (file:filename,charno:charno) : int * int =
    let	fun find ((source as {fileName,...}:Source.inputSource,_,_)::rest) =
	      if file = fileName then 
		let val (_,line,pos) = Source.filepos source charno
	        in (line,pos)
		end
	      else find rest
	  | find nil = (0,0)
    in find (!currentLocIndex)
    end
		  
  fun hideFile (file:filename) =
    let fun zap ((f as (inputSource:Source.inputSource,_,cuds))::rest) = 
	      if file = #fileName inputSource then
		(app hideCud cuds;
		 rest)
	      else f::(zap rest)
	  | zap nil = nil
    in currentLocIndex := zap (!currentLocIndex) 
    end 
  
  fun argCnt evt =
    case evt of 
      VALev(MARKdec(VALdec vbl,_,_)) => length (vblextract (fn x => x) vbl)
    | VALRECev(MARKdec(VALRECdec rvbl,_,_)) => length rvbl
    | FNev(RULE(pat,_)) => length (patvars (fn x => x) pat)
    | HANDLEev(RULE(pat,_)) => length (patvars (fn x => x) pat)
    | CASEev(_,RULE(pat,_)) => length (patvars (fn x => x) pat)
    | STRev(MARKdec(STRdec strbl,_,_)) => 1 + length strbl
    | ABSev(MARKdec(ABSdec strbl,_,_)) => 1 + length strbl
    | FCTev(MARKdec(FCTdec fctbl,_,_)) => length fctbl
    | EXCEPTIONev(MARKdec(EXCEPTIONdec ebl,_,_)) => length ebl
    | FCTAPPev _ => 1
    | SIGENDev => 1
    | ENDev (_) => 1
    | _ => 0

  (* Create a new cud and locIndex and install them tentatively. *)
  fun install {inputSource:Source.inputSource,
	       firstPlace:place,events: event vector, 
	       evns: place vector vector}  : unit =
    let (* compute and store list of evns immediately succeeding each event.
	   with present instrumentation algorithm, these lists
	   will be singletons (?) -- but this might change. *)
	val evnsForEvent =
	  let val a = array(Vector.length events,nil:evn list)
	      fun loop (n,evn) = 
		  let val places = listofvector(Vector.sub(evns,n))
		      val pevents = 
			  map (fn place => Vector.sub(events,place-firstPlace))
			       places
		      val places' = immediatePlaces'(places,pevents)
		  in app (fn place => update(a,place-firstPlace,
					     evn::(a sub (place-firstPlace))))
	 	        places';
		     loop(n+1,evn+1)
		  end
	  in loop (0,firstPlace) handle Vector.Subscript => a
	  end
	val cud = {inputSource=inputSource,
		   visible=ref true,
		   firstPlace=firstPlace,
		   events=events,
		   evns=evns,
		   evnsForEvent=evnsForEvent,
		   evnTimes=array(Vector.length evns,0)}
    in
      lastCud := !currentCud;
      lastLocIndex := !currentLocIndex;
      currentCud := cud;
      addCud cud;
      currentLocIndex := augmentLocIndex(!currentLocIndex,cud);
      if (!debugdebug) then
	(print (implode["Entering cud ",makestring firstPlace," ",
			makestring(Vector.length evns)," ",
			makestring(Vector.length events),"\n"]);
	 dumpEvns (firstPlace,evns,events);
	 dumpEvents(firstPlace,events,evnsForEvent))
      else ()
    end

  (* execute this when tentatively installed  units fail. *)
  fun rollback() = (currentCud := !lastCud;
		    currentLocIndex := !lastLocIndex)

  (* query functions *)
  fun nextPlace() = #firstPlace(!currentCud) + 
                    Vector.length(#events (!currentCud))

  fun lastUnitTime() = 
      lastTime(#firstPlace(!currentCud) + 
	       Vector.length(#evns(!currentCud)) - 1)

end (* structure DebugStatic *)
