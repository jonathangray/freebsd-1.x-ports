(* DebugRun

   Manages state caches and provides more convenient versions of some
   DebugExec functions; meant to be used together with DebugExec.

*)
   
signature DEBUG_RUN =
sig
  type time
  (* Motion functions.  These functions combine replay and record as 
     appropriate, so they can be used from any established state. *)
  val completeCompUnit: unit -> 'a    (* Does not return *)
  val advanceTo: time * bool (* precache? *) * DebugExec.onNoise -> unit
      (* Advance to specified time, known or unknown. 
         Returns when target or terminating/noisy event reached, 
	 or on interrupt.
	 Typically caches state reached before returning.
 	 If precache arg true, precaches a state somewhat before target. *)
  val resetTo: time * bool (* precache ? *) * DebugExec.onNoise -> unit
      (* Jump to known time, in past or future.
         Returns when target or noisy event reached, or on interrupt.
	 Typically caches state reached before returning.
	 If precache arg true, precaches a state somewhat before target when
	 target is far enough in future. *)
  val bracketCache: (unit->bool) -> (time * time)
      (* Searching forward from current time, return times of last cache entry
         for which condition is false, and first entry for which it is true
	 or infinity if no such. *)
  val zapRun: time -> unit
      (* Remove all cache and memo entries >= time. *)
  (* Event query functions.  On interrupt, they raise QueryInterrupted, but
     leave pendingInterrupt flag set. 
     These are here to allow alternative caching mechanisms to support them. 
     Supported by memoization if enabled. *)
  exception QueryInterrupted
  type evn
  val evnAt: time -> evn
  val lbtAt: time -> time
  val evnLbtAt: time -> evn * time
  val argsAt : time -> System.Unsafe.object list
  val evnArgsAt: time -> evn * System.Unsafe.object list
  val evnLbtArgsAt: time -> evn * time * System.Unsafe.object list
  (* Control / Debug *)
  val maxTimeDelta : int ref
      (* Maximum allowed execution interval.  Intended to keep store list
         space overhead under control. *)
  val maxStates: int ref (* in cache *)
  val memoLevel: int ref 
  val dumpCache: unit -> unit (* for debugging *)
  val dfactor : real ref
  val preCachingEnabled : bool ref 
  val pcfactor : real ref
  val zapFactor : real ref
  val strictLru : bool ref
  val cacheRatio : int ref
  val zapCount : int ref  (* number of states zapped to make room *)
end

structure DebugRun:DEBUG_RUN =
struct
  open Array List DebugUtil DebugStatic DebugExec
  infix 9 sub
  structure U = System.Unsafe

  val zapCount = ref 0

  (* The memo table *)
  val memoLevel = ref 0  (* meaning: 0 = no memoization
			             1 = memoize evn,lbt
				     2 = memoize evn,lbt,args *)

  type entry = {evn:evn,lbt:time,args:U.object array option}
  exception NoMemo
  val memoMap:entry Intmap.intmap = Intmap.new(32,NoMemo)

  fun memoize (state:state) =
    if !memoLevel > 0 then
      let val time = #time state
	  val {evn,lbt,args} = #evData state
          val entry = 
	     if !memoLevel > 1 then
	       {evn=evn,lbt=lbt,args=SOME(arrayoflist args)}
	     else {evn=evn,lbt=lbt,args=NONE}
      in Intmap.add memoMap (time,entry)
      end
    else ()

  fun recall time = Intmap.map memoMap time

  fun zapMemo time = (* remove all memos for times >= arg *)
     let val z:time list ref = ref nil
	 fun addif (t,_) = if t >= time then z := t ::(!z) else ()
     in Intmap.app addif memoMap;
	app (Intmap.rmv memoMap) (!z)
     end

  (* The state cache *)
  
  local 
    structure StateSet = SortedSet(
      struct
        type t = state * int (* use number *)
        type k = time
	fun key ((s,_):t) = #time s
	val lt = Integer.<
      end)
    val states = ref (StateSet.new())
    fun insertState(state,usenum) = 
	states := StateSet.insert(!states,(state,usenum))
    fun updateState(state,usenum) =
	states := StateSet.update(!states,(state,usenum))
    fun findState time = StateSet.find(!states,time)
    fun findpState time = StateSet.findp(!states,time)
    fun findsState time = StateSet.finds(!states,time)
    fun findcState cond = StateSet.findc(!states,cond)
    fun deleteState time = states := StateSet.delete(!states,time)
    fun numStates() = StateSet.size(!states)
    fun iterateStates f = StateSet.iterate(!states,f)

    structure UseHeap = SortedSet(
      struct
        type t = int (* use number *) * time				 
	type k = int
        fun key((u,_):t) = u
        val lt = Integer.<
      end)
    val uses = ref(UseHeap.new())
    fun insertUse (unum,time) = 
	if (unum > 0) andalso (time > 0) then
	    uses := UseHeap.insert(!uses,(unum,time))
	       handle UseHeap.DuplicateKey => debugPanic "Run.insertUse"
        else ()
    fun deleteUse unum =
	uses := UseHeap.delete(!uses,unum)
	    handle UseHeap.NotFound => ()
    fun updateUse (unum,time) = 
	if (unum > 0) andalso (time > 0) then
	    uses := UseHeap.update(!uses,(unum,time))
	       handle UseHeap.NotFound => debugPanic "Run.updateUse"
	else ()
    fun findsUse unum = UseHeap.finds(!uses,unum)
    fun iterateUses (time,f) = UseHeap.iteratefrom(!uses,time,f) 
	    

    val usenum = ref 0	(* use counter for maintaining LRU *)
  in 
  
  val dfactor = ref 0.25 
  val maxStates = ref infinity	(* maximum number of states to maintain *)
  val preCachingEnabled = ref false
  val pcfactor = ref 0.25

  fun findPrevState time = 
    let val (pstate,lastusenum) = findpState time
    in inc usenum;
       updateState(pstate,!usenum);
       deleteUse lastusenum;
       insertUse (!usenum,#time pstate);
       pstate
    end handle StateSet.NotFound => zeroState

  fun findSuccState time =
    let val (sstate,lastusenum) = findsState time
    in inc usenum;
       updateState(sstate,!usenum);
       deleteUse lastusenum;
       insertUse (!usenum,#time sstate);
       SOME sstate
    end handle StateSet.NotFound => NONE

  fun findCondState cond =
    let val (cstate,_) = findcState cond
    in SOME cstate
    end handle StateSet.NotFound => NONE

  fun zapCache time  = (* remove all states for times >= arg *)
    let fun zap () =
      let val (s,lastusenum) = findsState time
      in deleteState (#time s);  
	 deleteUse lastusenum;
	 dbgprint ("*z " ^ (makestring (#time s)) ^ "\n");
	 (* throw away use information in zapped states! *)
	 zap()
      end handle StateSet.NotFound => ()
    in zap()
    end
  
  val cacheRatio = ref 5

  fun moreRoom () = (* decide whether there's room to cache another state *)
    let open System.Control.Runtime
    in !lastratio >= (!cacheRatio * 100) andalso numStates() < !maxStates
    end

  val nextZap = ref 0  (* next use number to try zapping *)
  val zapD = ref 1     (* d value at last zap *)
  val strictLru = ref false 

  fun zapLru0 (d,t0) = (* find an lru state whose delta is <= d; include t0 *)
      let exception Found of time * int * state * int
	  fun find (use,t) = 
	      let val _ = assert(t > 0, "Run.zapLru0 t <= 0")
		  val (pstate,plastuse) = 
		   findpState (t-1)
		      handle StateSet.NotFound => (zeroState,0)
		  val ptime = #time pstate
		  val delta = if t > t0 andalso t - t0 < t - ptime then
		                t - t0
			      else t - ptime
	      in if delta <= d orelse !strictLru then
		   (dbgprint(implode["*z t=",makestring t," u=",makestring use,
				     " pt=", makestring ptime,
				     " pu=", makestring plastuse,
				     " delta=", makestring delta, "\n"]);
		    inc zapCount;
		    raise Found (t,use,pstate,plastuse))
                 else ()
              end
      in (iterateUses (!nextZap,find);
	  dbgprint "*zapLru recycle\n";
	  iterateUses (0,find);
	  debugPanic "Run.zapLru can't find LRU state to zap")
	    handle Found (t,use,pstate,plastuse) => 
		 (deleteState t handle StateSet.NotFound => 
		      debugPanic "Run.zapLru deleteState";
		  if #time pstate > 0 andalso use > plastuse then
		    (updateState(pstate,use);
		     deleteUse plastuse;
		     updateUse (use,#time pstate))
		  else deleteUse use;
		  nextZap := use + 1)
      end

 
  val zapFactor = ref 1.00
  fun zapLruN (d,t0) = (* delete an approp. number of lru states and re-gc *)
      let open System.Control.Runtime
	  val keep = real(!lastratio) / (real(!cacheRatio * 100))
	  val toss = max(0,floor((1.0 - keep) * real(numStates())))
	  val n = floor(real toss * (!zapFactor)) 
	  val _ = dbgprint ("*zapLruN " ^ (makestring n) ^ "\n")
	  fun find (use0,0) = 0
	    | find (use0,i) = 
	      let val (use,t) = findsUse (use0+1) 
	          val _ = assert(t > 0, "Run.zapLruN t <= 0")
		  val (pstate,plastuse) = 
		      findpState (t-1)
		      handle StateSet.NotFound => (zeroState,0)
		  val ptime = #time pstate
		  val delta = if t > t0 andalso t - t0 < t - ptime then
		                t - t0
			      else t - ptime
	      in if delta <= d orelse !strictLru then
		   (dbgprint(implode["*z t=",makestring t," u=",makestring use,
				     " pt=", makestring ptime,
				     " pu=", makestring plastuse,
				     " delta=", makestring delta, "\n"]);
		    inc zapCount;
		    deleteState t handle StateSet.NotFound => 
		        debugPanic "Run.zapLru deleteState";
   	  	    if #time pstate > 0 andalso use > plastuse then
		      (updateState(pstate,use);
		       deleteUse plastuse;
		       updateUse (use,#time pstate))
		    else deleteUse use;
		    nextZap := use + 1;
		    find (use,i-1))
		 else find (use,i)
              end handle UseHeap.NotFound => i
      in if n > 0 andalso find(!nextZap,n) < n then
	   U.CInterface.gc 1 (* force major collection *)
	 else zapLru0 (d,t0)
(*	 else (find(!nextZap,1);()) *)
      end


  fun zapLru (d,t0) =
     (if d >= 2 * (!zapD) then
	(nextZap := 0;
	 zapD := d)
      else ();
      zapLruN (d,t0);
      moreRoom())

  fun calcD () =
    let val maxStateTime = ((#time o #1) (findpState infinity))
	                                    handle StateSet.NotFound => 1
    in if numStates() > 0 then maxStateTime div (numStates()) else 1
    end

  fun saveState (state:state) = 
    let val time = #time state
	fun doInsert () = 
	    (inc usenum;
	     insertState(state,!usenum) handle StateSet.DuplicateKey => ();
	     insertUse(!usenum,time);
	     (* could revise nextZap here if needed:
	         let succ = successor to current state
		 in if succ's usenum < nextZap andalso
		     (time succ - time < d) then
		     reset nextZap to succ's usenum
		 end *)
	     dbgprint ("*i " ^ (makestring time) ^ "\n"))
        fun doRemove ()  = 
          let val d = calcD()
	      val d' = floor(real d * (!dfactor))
	      val (pred,predlastuse) = 
		   findpState time handle StateSet.NotFound => 
		       (zeroState,0)
	      val predTime = #time pred
	  in if (time - predTime) > d' then
	       let val (succ,succlastuse) = findsState time
		   val succTime = #time succ
	       in if succTime - time < d' then
		   ((* zap successor *)
		    deleteState succTime;
		    deleteUse succlastuse;
		    dbgprint ("*z t=" ^ (makestring succTime) ^ 
			      " delta=" ^ (makestring (succTime-time)) ^ "\n");
		    true)
		  else zapLru (d,time) 
	       end handle StateSet.NotFound => zapLru (d,time) 
	     else 
	       (if predTime > 0 then 
	  	  ((* just update pred's use info *)
	 	  inc usenum;
		  updateState(pred,!usenum)
		     handle StateSet.NotFound => 
			 debugPanic "Run.saveState.doReplace.update pred";
		  deleteUse predlastuse;
		  insertUse(!usenum,predTime))
		else ();
		false)
	  end
    in dbgprint ("*s " ^ (makestring time) ^ "\n");
       memoize state;
       if time = 0 then
	 ()
       else if moreRoom() then
	 doInsert()
       else if numStates() > 0 then
	   (if doRemove() then
	      doInsert()
	    else ())
       else ();
       (!sizereport) ("*s" ^ (makestring time))
    end

(*
  fun saveState1 (state:state) = 
    let val time = #time state
	fun doInsert () = 
	    (inc usenum;
	     insertState(state,!usenum) handle StateSet.DuplicateKey => ();
	     insertUse(!usenum,time);
	     (* could revise nextZap here if needed:
	         let succ = successor to current state
		 in if succ's usenum < nextZap andalso
		     (time succ - time < d) then
		     reset nextZap to succ's usenum
		 end *)
	     dbgprint ("*i " ^ (makestring time) ^ "\n"))
        fun doRemove ()  = 
          let val d = calcD()
	      val d' = floor(real d * (!dfactor))
	      val (pred,predlastuse) = 
		   findpState time handle StateSet.NotFound => 
		       (zeroState,0)
	      val predTime = #time pred
	  in if (time - predTime) > d' then
	       let val (succ,succlastuse) = findsState time
		   val succTime = #time succ
	       in if succTime - time < d' then
		   ((* zap successor *)
		    deleteState succTime;
		    deleteUse succlastuse;
		    dbgprint ("*z t=" ^ (makestring succTime) ^ 
			      " delta=" ^ (makestring (succTime-time)) ^ "\n");
		    true)
		  else moreRoom() orelse zapLru (d,time) 
	       end handle StateSet.NotFound => 
		                moreRoom() orelse zapLru (d,time) 
	     else 
	       (if predTime > 0 then 
	  	  ((* just update pred's use info *)
	 	  inc usenum;
		  updateState(pred,!usenum)
		     handle StateSet.NotFound => 
			 debugPanic "Run.saveState.doReplace.update pred";
		  deleteUse predlastuse;
		  insertUse(!usenum,predTime))
		else ();
		false)
	  end
    in dbgprint ("*s " ^ (makestring time) ^ "\n");
       memoize state;
       if time = 0 then
	 ()
       else if numStates() > 0 then
	   (if doRemove() then
	      doInsert()
	    else ())
       else if (moreRoom()) then
	 doInsert()
       else ();
       (!sizereport) ("*s" ^ (makestring time))
    end
*)
	  
  fun dumpCache () =
     (print "Cache contents:\nTime\tUse\n";
      iterateStates (fn (s,u) => 
		      (print (#time s); print "\t"; print u; print "\n"));
      print "D = "; print (calcD()); print "\n")

  end (* let open structure ... *)
  
  fun zapRun t = 
      (zapCache t;
       zapMemo t)

  fun saveCurrentState state : unit = saveState (currentState())
      (* N.B. doesn't displace existing state. *)

  fun restoreBestPrev (time,onNoise) =
      let val best = findPrevState time
      in if (time > currentTime() andalso #time best > currentTime())
	     orelse (time < currentTime()) then
	   restoreState(best,onNoise)
         else ()
      end

  (* Time-travel functions.  These operate on arbitary times, and
      hide current state, histories, and caches.
      They optionally implement pre-caching strategies, etc.
      All such policy decisions are at this level. *)

  val maxTimeDelta = ref 100000 (* must be > 0 *)

  fun completeCompUnit () =  (* doesn't return *)
   (* N.B. Doesn't behave properly wrt/ maxTimeDelta ... *)
      (dbgprint ("*com\n");
       restoreBestPrev (infinity,NOISY);
       if currentTime() < knownTime() then
	 (replayTo (knownTime(),NOISY);
	  if (!pendingInterrupt) then
	    interruptCompUnit()  (* does not return *)
	  else())
       else ();
       recordRest () (* does not return *))
    
  fun advanceTo (target:time,preCache:bool,onNoise:onNoise) =
    (* Returns when target or terminating event is reached, onNoise condition
       occurs, halting signal occurs, 
       or on interrupt (in which case interruptPending is set).
       If preCache is true, stores a nearby earlier state. *)
     (dbgprint ("*adv " ^ (makestring target) ^ 
	       (if preCache then " p\n" else "\n"));
       assert (target >= currentTime(),"Run.advanceTo");
       let fun go t =
	         (currentTime() >= knownTime() orelse
		    (replayTo(min(knownTime(),t),onNoise) andalso
		     not (!pendingInterrupt))) andalso
		 (currentTime() = t orelse
		    (recordTo (t,onNoise) andalso
		     not (!pendingInterrupt) andalso
		     not (DebugSignals.deliverableHalting()))) andalso
		 (saveCurrentState(); true)
           fun go' t =
	       let val t' = min(t,currentTime() + !maxTimeDelta)
	       in (currentTime() = t') orelse
		   (go t' andalso go' t)
               end
       in if currentTime() < target then
	   ((case onNoise of
	       BREAK _ => ()  (* must re-execute here *)
	     | _ => restoreBestPrev (target,onNoise));
	    if preCache andalso !preCachingEnabled then
	       let val target' = target - floor(real(calcD()) * (!pcfactor))
		   val target' = max(currentTime(),target')
	       in go' target' andalso go' target;
		  ()
	       end
            else (go' target;()))
	  else ()
       end)

  fun resetTo (target:time,preCache:bool,onNoise:onNoise) =
      (* Returns when target reached, onNoise condition occurs,
         or interrupt is set (in which case interruptPending is set).
	 If preCache is true, store a nearby earlier state.
	 OnNoise must be QUIET for target < currentTime(). *)
      (dbgprint ("*reset " ^ (makestring target) ^ 
		 (if preCache then " p\n" else "\n"));
       assert (target <= knownTime(),"Run.resetTo unknown target");
       assert (case onNoise of
		 QUIET => true
	       | _ => target >= currentTime(),"Run.resetTo bad onNoise");
       let fun go t =
	         (replayTo(t,onNoise) andalso
		  not(!pendingInterrupt) andalso
		  (saveCurrentState();true))
           fun go' t =
	       let val t' = min(t,currentTime() + !maxTimeDelta)
	       in (currentTime() = t') orelse
		   (go t' andalso go' t)
               end
       in if target <> currentTime() then
	    (restoreBestPrev(target,onNoise);
	     if preCache andalso !preCachingEnabled then
	       let val target' = target - floor(real(calcD()) * (!pcfactor))
		   val target' = max(currentTime(),target')
	       in go' target' andalso go' target;
		  ()
	       end
	     else (go' target;()))
	  else ()
       end)

  fun bracketCache (condition:unit -> bool) : time * time =
    (* Searching forward from current time, return times of last cache entry
       for which condition is false (or current time if no such), 
       and first entry for which it is true or infinity if no such.  
       If interrupt occurs, return (original current time, infinity).
       A special-purpose version of this function in which condition was
       based only on eventTimes could be much cheaper!
    *)
     (assert (not (condition()), "Run.bracketCache");
      let val origState = currentState()
	  val origTime = currentTime()
	  fun cond state =
	         #time state > origTime andalso
	         (restoreState(state,QUIET);
		  !pendingInterrupt orelse (condition()))
          val ub = 
	     case findCondState (fn (state,_) => cond state) of
	       SOME ubstate =>
	     (* N.B. There's no guarantee that the ubstate is still in the
	        cache at this point, because evaluating condition() may 
		have altered cache contents. *)
	         (case findSuccState (#time ubstate) of
		    SOME state => #time state
		  | NONE => infinity)
	     | NONE => infinity
	  val (lb,ub) = 
	      let fun loop t =
		    if !pendingInterrupt then
		      (origTime,infinity)
		    else
 		      let val state = findPrevState(t-1)
			  val time = #time state
		      in if cond state then
			   loop time
			 else (max(time,origTime),t)
		      end
	      in loop ub
	      end
      in restoreState (origState,QUIET);
	 (lb,ub)
      end)


  (* checkResetTo is suitable for support functions that do time-travel
     without restoring, but are vulnerable to interrupts. *)
  exception QueryInterrupted
  fun enforce time: unit =
    if currentTime() <> time then
      (if (!pendingInterrupt) then
	 raise QueryInterrupted
       else debugPanic ("Run.enforce " ^ 
			(makestring time) ^ " " ^
			(makestring (currentTime()))))
    else ()
  fun checkResetTo time = (resetTo (time,true,QUIET); enforce time)
  
  fun evnAt (time:int) = 
      #evn(recall time)
       handle NoMemo => (checkResetTo time;
			 currentEvn())
  
  fun lbtAt (time:int) =
      #lbt(recall time)
       handle NoMemo => (checkResetTo time;
			 currentLbt())

  fun evnLbtAt (time:int) = 
      let val {evn,lbt,...} = recall time
      in (evn,lbt)
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentLbt()))

  fun argsAt (time:int) =
     (case #args (recall time) of
        SOME argarr => ArrayExt.listofarray argarr
      | NONE => raise NoMemo)
       handle NoMemo => (checkResetTo time;
			 currentArgs())

  fun evnArgsAt (time:int) =
      let val {evn,args,...} = recall time
      in case args of
	   SOME argarr => (evn,ArrayExt.listofarray argarr)
	 | NONE => raise NoMemo
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentArgs()))
 
  fun evnLbtArgsAt (time:int) =
      let val {evn,lbt,args} = recall time
      in case args of
	   SOME argarr => (evn,lbt,ArrayExt.listofarray argarr)
	 | NONE => raise NoMemo
      end
       handle NoMemo => (checkResetTo time;
			 (currentEvn(),currentLbt(),currentArgs()))

end

