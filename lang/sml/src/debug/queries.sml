(* DebugQueries

   User-level queries.
   By convention, these are all relative to an explicitly given time.
   They should only be called when imbedded in some DebugMotion routine
   (e.g., withEstablishedTime or binSearch) that ensures interrupt handler
   is set, established time is reset, etc.
   On interrupt, these routines return something dumb, and expect an outer
   wrapper to do something sensible when they notice the flag is still set.
*)


signature DEBUG_QUERIES =
sig
  type time
  type place 
  type wherewhen (* = place * time *)
  val caller: time -> (wherewhen*wherewhen) 
      (* Return top of function,caller for given time. *)
  val callTrace: int -> time -> 
                 ((wherewhen*wherewhen*(((string*Types.ty)*System.Unsafe.object) list)) list)
      (* Return specified number of call frames from given time.
         Each frame gives top of function, caller, and list of 
	   ((name*type)*value) for each function argument bound at the call. *)
  val eventDesc : place -> (string * bool * DebugStatic.location * DebugStatic.visible) option
      (* Return descriptive string, pseudo-ness, location, 
         and visibility attributes for given place. *)
  val lastTimes : place -> time * time
      (* Return (min,max) of last times for evns including place. *)
  val atCall : time -> bool
      (* Return true iff time refers to a call point (APP, etc.) *)
  val getVal: string -> time -> 
                  (System.Unsafe.object * Types.ty * wherewhen) option
      (* Return value*type*binding site for given val identifier looking
         backwards from given time. *)
  val printBind: (wherewhen * int) -> unit
      (* Print binding information for binding at given wherewhen. *)
  val printVal: (System.Unsafe.object * Types.ty) -> unit
  val exnArg: exn -> (System.Unsafe.object * Types.ty) option
end

structure DebugQueries : DEBUG_QUERIES =
struct
  (* These functions should be imbedded (directly or at some deeper level)
     within some X motion routine, to ensure the interrupt handler is set.
     Policy on interrupts: returns something foolish and relies on caller
     to re-detect pendingInterrupt. *)

  open DebugUtil DebugRun DebugBindings DebugStatic DebugExec Modules
       Access Absyn Types PrintUtil Variables
  structure U = System.Unsafe

  type wherewhen = place * time

  val say = System.Print.say

  val pdepth = 100
 
  (* Suitable for support functions that do time-travel and may be 
     called from within time-sensitive contexts. Callers must
     check of pendingInterrupt on their return. *)
  fun keepOriginalTime f =
    let val originalTime = currentTime()
    in f() before
       (if not (!pendingInterrupt) then
 	  resetTo (originalTime,true,QUIET)
        else ()) (* caller will need to deal with interrupt anyhow *)
    end

  val eventsAt = eventsFor o evnAt

  fun currentWhereWhen () = (hd(placesFor(currentEvn())),currentTime())
    
  fun caller (time:time) : (wherewhen*wherewhen) =
      (* return top of function containing time, caller of this function *)
    keepOriginalTime (fn () =>
	let fun lastfunc(0,(evn,_)) = (0,evn,0)
	      | lastfunc(t,(evn,lbt)) =
		   let fun f (FNev(_)::_,n) = (t,evn,n)
			 | f (HANDLEev(_)::_,n) = (t,evn,n)
			 | f (evt::rest,n) = f(rest,n+1)
			 | f (nil,_) = lastfunc(lbt,evnLbtAt lbt)
		   in if !pendingInterrupt then
		        raise QueryInterrupted
		      else f (eventsFor evn,0)
		   end
	    val (t,evn,n) = lastfunc(time,evnLbtAt time)
	    val (t',evn') = if t > 0 then
		              let val pt = pred t
			      in (pt,evnAt pt)
			      end
			    else (0,0)
	in ((nth(placesFor evn,n),t),(hd(placesFor evn'),t'))
	end handle QueryInterrupted => ((0,0),(0,0)))

  fun eventDesc (place:place) :
         (string * bool * location * visible) option =
    let val evt = eventForPlace place
	val (filename,visible) = filenameFor place
	val charno = locOfEvent evt
    in SOME (eventText evt,isPseudo evt,(filename,charno),visible)
    end handle Nth => NONE

  fun lastTimes (place:place) : time * time =
      (* return (min,max) of last times for evn's containing place. *)
      let fun check(evn,(mint,maxt)) =
	  let val t = lastTime evn
	  in (min(mint,t),max(maxt,t))
	  end
      in fold check (evnsFor place) (infinity,~infinity)
      end

  fun atCall (t:time) =
    case hd (eventsAt t) of
      (APPev _) => true
    | (RAISEev _) => true
    | _ => false

  (* Routine getVal supports looking up VARCON's using full path names;
     returns with unbundled value, type and binding site information option.
   Intended use: supporting pointing at source. 
   Sample packaging:  
    getVal' (n:string) = withEstablishedTime(getVal n)
   with the INTERRUPT x return assumed corrupt. *)

  local
     fun split s =
	 let fun sp s =
	     let val pos = index (fn c => c = ".") (explode s)
	     in substring (s,0,pos) :: 
		 sp (substring(s,pos+1, String.length s - (pos+1)))
	     end handle Index => [s]
	 in rev (sp s)
	 end
  in
  fun getVal (n:string) (time:time) : 
                     (U.object * ty * wherewhen) option  =
      keepOriginalTime (fn () =>
      let val (t,c,(i,binding)) = findVARCONBind (split n, time,0) 
      in case binding of
	   VARbind(v as VALvar{typ=ref ty,...}) =>
	     let val (evn,args) = evnArgsAt t
 	         val bv = nth (nthArgs(evn,c,args),i)
	         val ty = dynTypeAt (t,c) ty
	         val ww = (nth(placesFor evn,c),t)
             in SOME(bv,ty,ww)
             end 
	 | VARbind (OVLDvar _) => NONE
	 | CONbind(dc as (DATACON{const,rep,sign,typ,...}))  =>
	     let val (evn,args) = evnArgsAt t
		 val ty = dynTypeAt (t,c) typ
		 val bv = case rep of
	  	  	     VARIABLE _ =>  nth(nthArgs(evn,c,args),i)
			   | VARIABLEc _ => nth(nthArgs(evn,c,args),i)
	   	   	   | _ => U.cast 0 
		                     (* no run-time object exists *)
		 val ww = (nth(placesFor evn,c),t)
	     in SOME(bv,ty,ww)
	     end 
      end handle Env.Unbound => NONE | QueryInterrupted => NONE)
  end  (* local *)




  local
    fun nametype (t,c) = 
	          fn (v as VALvar{name=[nm],typ=ref ty,...}) =>
			(Symbol.name nm, 
			 fn () => dynTypeAt (t,c) ty)
		  | _ => debugPanic "bad var in queries.nametype"

  in
  fun callTrace (maxdepth:int) (time:time):
      ((wherewhen*wherewhen*(((string*ty)*U.object) list)) list) = 
    keepOriginalTime (fn () => 
	let fun dotype (name,typef) = (name,typef())
	    fun lastfunc(0,(evn,_,_)) = (0,evn,0,nil)
	      | lastfunc(t,(evn,lbt,args)) =
		   let fun f (FNev (RULE(pat,_))::_,n,args) = 
			      (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
			 | f (HANDLEev (RULE(pat,_))::_,n,args) =
			      (t,evn,n,pairlist (patvars (dotype o (nametype (t,n))) pat) args)
			 | f (evt::rest,n,args) = f(rest,n+1,tln (args,argCnt evt))
  
			 | f (nil,_,_) = lastfunc(lbt,evnLbtArgsAt lbt)
		   in f (eventsFor evn,0,args)
		   end
	    fun up 0 _ = nil
	      | up _ 0 = nil
	      | up t d =
		   let val evdata as (evn,_,_) = evnLbtArgsAt (pred t)
		       val (t',evn',n,varlist) = lastfunc(pred t,evdata)
		   in
		     ((hd(placesFor evn),pred t),
		      (nth(placesFor evn',n),t'),
		      varlist) :: 
		     (up t' (d-1))
		   end
	    val cww = currentWhereWhen()
	    val (t',evn',n,varlist) = lastfunc (time,evnLbtArgsAt time)
	in (cww,(nth(placesFor evn',n),t'),varlist) :: (up t' maxdepth) 
	      handle QueryInterrupted => nil  
	           (* could do better if we really wanted to *)
	end)
  end (* local *)

  fun printBind ((place:place,t:time),indent:int) : unit =
   keepOriginalTime (fn () => 
    let val env = StaticEnv.atop(#static(DebugEnv.debugEnvironment),
			       !debugStatEnv)
	open PrettyPrint
	val consumer = ErrorMsg.defaultConsumer()
	fun printDec arg =
	    with_pp consumer (fn ppstrm => PPAbsyn.ppDec (env,NONE) ppstrm arg)
    in	     
     case eventForPlace place of
       VALev(MARKdec(dec as VALdec(_),_,_)) => printDec (dec,pdepth)
     | VALRECev(MARKdec(dec as VALRECdec(_),_,_)) => 
	   printDec (dec,pdepth)
     | FNev(RULE(pat,_)) => 
         with_pp consumer (fn ppstrm => 
	  (PPAbsyn.ppPat env ppstrm (pat,pdepth);
	   add_string ppstrm " <=== ";
	   case hd(eventsAt (pred t))
	     of APPev(APPexp(_,exp)) =>
		  PPAbsyn.ppExp (env,NONE) ppstrm (exp,pdepth)
	      | _ => add_string ppstrm  "unknown call site"))
     | HANDLEev(RULE(pat,_)) =>
         with_pp consumer (fn ppstrm => 
	  (PPAbsyn.ppPat env ppstrm (pat,pdepth);
	   add_string ppstrm " <=== ";
	   case hd(eventsAt (pred t))
	     of RAISEev(MARKexp(RAISEexp(exp,_),_,_)) => 
		  PPAbsyn.ppExp (env,NONE) ppstrm (exp,pdepth)
	      | _ => add_string ppstrm "implicit exception"))
     | CASEev(exp,RULE(pat,_)) => 
         with_pp consumer (fn ppstrm => 
	  (PPAbsyn.ppPat env ppstrm (pat,pdepth);
	   add_string ppstrm " <=== ";
	   PPAbsyn.ppExp (env,NONE) ppstrm (exp,pdepth)))
     | _ => debugPanic "bad event type in queries.printBind"
    end handle QueryInterrupted => ())

   fun printVal(v,t) = 
      let val env = StaticEnv.atop(#static(DebugEnv.debugEnvironment),
				 !debugStatEnv)
      in  PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
	    (fn ppstrm => 
	      PPVal.ppVal env ppstrm (v,t,!System.Print.printDepth))
      end

   (* Return argument and type associated with an exception, if any; else NONE.
    Also return NONE if exception declaration was not instrumented for
    debugging. *)
   local
     open System.Tags
     val string_tag = make_desc(0,tag_string)
     and embedded_string_tag = make_desc(0,tag_embedded_string)
   in
   fun exnArg (exn:exn) : (U.object * ty) option =
     let val (ref s,v) = U.cast exn
     in if not(U.boxed s) orelse 
	   let val tag = U.getObjTag s 
	   in tag = string_tag orelse tag = embedded_string_tag 
	   end 
	then
	  NONE (* non-debugger exception *)
	else
	  let val (name0:string,t:time) = U.cast s
	      fun find ((EBgen{exn=DATACON{name,...},etype,...})::rest) =
		         if name0 = Symbol.name name then
			   case etype of
			     SOME ty => SOME(v,dynTypeAt (t,0) ty)
			   | NONE => NONE
			 else find rest
		| find (_::rest) = find rest
		| find nil = debugPanic "no matching name in queries.exnArg"
	  in keepOriginalTime
	      (fn () => 
	        (case hd(eventsAt t) of
		   EXCEPTIONev(MARKdec(EXCEPTIONdec ebl,_,_)) => find ebl
	         | _ => debugPanic "bad event type in queries.exnArg")
		     handle QueryInterrupted => NONE)
	  end
     end
   end
end



