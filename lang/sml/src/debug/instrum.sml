(* DebugInstrum

   Instrument user code.
   Instrumented code needs to gain access to a variety of structures at 
     run-time, some of them in a dirty fashion.  This is done via
     a special system ref set up in DebugInterface.

*)

signature DEBUGINSTRUM = sig
   val instrumDec: {absyn:Absyn.dec,
		    firstPlace:DebugStatic.place,  (* = firstEvn *)
		    lastBindTime:DebugStatic.time} ->
         {absyn:Absyn.dec,
	  events:DebugStatic.event Vector.vector,
	  evns:DebugStatic.place Vector.vector Vector.vector}
   val instrumLevel : int ref
end

structure DebugInstrum : DEBUGINSTRUM =
struct

open Vector Array List DebugUtil DebugStatic Access Absyn 
ElabUtil BasicTypes ErrorMsg Variables Types Modules
infix 9 sub
   
val instrumLevel = ref 3 
(* Controls instrumentation method.  Possible values:
   2 - simplistic:
         - no sequentialization analysis; 
	 - event at top of every function
   3 - a limited sequentialization approach
   4 - and its variant.
*)


val makevar = mkVALvar o Symbol.varSymbol

local
val count = ref 0
in
fun makenvar str =
     makevar(str ^ makestring(!count))
     before inc count
fun makenstrvar str =
     let val name = Symbol.strSymbol (str ^ makestring(!count))
		    before inc count	      
	 val lvar = namedLvar name
     in STRvar{name=name,access=PATH[lvar],binding=ERROR_STR}
     end
end


(* Generate a "use" for an object with the given lvar, which need not be
   a simple object. *)
fun fakeuse(name,lvar) = 
	VARexp(ref (VALvar{name=[name],access=PATH[lvar],typ=ref UNDEFty}),
               NONE)


(* val ubassop = VALvar{name= [Symbol.varSymbol "unboxedassign"],
		    access=INLINE(P.unboxedassign),
		    typ=ref UNDEFty} *)
val assop = VALvar{name=[Symbol.varSymbol ":="],
		   access=INLINE(P.ASSIGN),
		   typ=ref UNDEFty}
val subop = VALvar{name=[Symbol.varSymbol "subscript"],
		   access=INLINE(P.SUBSCRIPT),
		   typ=ref UNDEFty}
val derefop = VALvar{name= [Symbol.varSymbol "!"],
		    access=INLINE(P.DEREF),
		    typ= ref UNDEFty}
val addop = VALvar{name=[Symbol.varSymbol "+"],
		    access=INLINE(P.IADD),
		    typ=ref UNDEFty}
val updateop = VALvar{name=[Symbol.varSymbol "unboxedupdate"],
		      access=INLINE(P.UNBOXEDUPDATE),
	 	      typ = ref UNDEFty}
(* val ineqop = VALvar{name=[Symbol.varSymbol "ineq"],
		    access=INLINE(P.INEQ),
		    typ = ref UNDEFty} *)
val ieqlop = VALvar{name=[Symbol.varSymbol "ieql"],
		    access=INLINE(P.IEQL),
		    typ = ref UNDEFty} 
val specialop = VALvar{name= [Symbol.varSymbol "mkspecial"],
                    access=INLINE(P.MKSPECIAL),
		    typ= ref UNDEFty}
(*
val useop = VALvar{name = [Symbol.varSymbol "uselvar"],
		   access=INLINE(P.USELVAR),
		   typ = ref UNDEFty}
val defop = VALvar{name= [Symbol.varSymbol "deflvar"],
                   access=INLINE(P.DEFLVAR),
		   typ = ref UNDEFty}
*)

fun instrumDec {absyn:dec,firstPlace:int,lastBindTime:time} =

let 
val nextPlace = ref firstPlace  (* next event ev to use *) 
val eventList = ref ([]: event list)  (* events generated *)
val nextEvn = ref firstPlace (* next evn to use *)
val evnList = ref ([]: place vector list) (* evns generated *)

fun makeEvent event : place =
  (eventList := event::(!eventList);
   !nextPlace before
   inc nextPlace)

fun makeEvn places : evn =
  (evnList := vector places::(!evnList);
   !nextEvn before
   inc nextEvn)

val timesvar =  makevar "_debugtimes"
val eventtimes = makevar "_eventtimes"
val breakentry = makevar "_breakentry"
val arrayvar = makevar "_array"

fun maketimearr len =
  let val timearr = makenvar "_timearr"
  in (VALdec[VB{pat=VARpat(timearr),
		exp=APPexp(VARexp(ref arrayvar,NONE),
			   TUPLEexp[INTexp len,
				    INTexp 0]),
		tyvars=ref nil}],
      VARexp (ref timearr,NONE))
  end

val GETTIMEexp = 
    APPexp(VARexp(ref subop,NONE), 
	   TUPLEexp[VARexp(ref timesvar,NONE),
		    INTexp 0])

fun INCexp exp = 
    APPexp(VARexp(ref addop,NONE),
	   TUPLEexp[exp,
		    INTexp 1])

fun makent label = 
  let val nt = makenvar label
  in (VALdec[VB{pat=VARpat nt,
		exp=INCexp(GETTIMEexp),
		tyvars=ref nil}],
      VARexp (ref nt,NONE))
  end


fun SETTIMEexp ntimeexp = APPexp(VARexp(ref updateop,NONE),
				 TUPLEexp[VARexp(ref timesvar,NONE),
					  INTexp 0,
					  ntimeexp])
							   

fun NOTETIMEexp(ntimeexp,evn) = APPexp(VARexp (ref updateop,NONE), 
			               TUPLEexp[VARexp(ref eventtimes,NONE),
				                INTexp (evn-firstPlace),
				       		ntimeexp])

fun BREAKexp(ntimeexp, evn, args) =
      CASEexp(APPexp(VARexp(ref ieqlop,NONE), 
		     TUPLEexp[ntimeexp, 
		              APPexp(VARexp(ref subop,NONE), 
				     TUPLEexp[VARexp(ref timesvar,NONE),
					      INTexp 1])]),
	      [RULE(TRUEpat, APPexp(VARexp (ref breakentry,NONE),
				    TUPLEexp(INTexp evn::args))),
	       RULE(FALSEpat, unitExp),
	       RULE(WILDpat, INTexp 0)])

fun EVENTexp (evn,lbt,args) = 
     let val (ntimedec,ntimeexp) = makent "_newtime"
     in LETexp (ntimedec,
		SEQexp [SETTIMEexp ntimeexp,
			BREAKexp(ntimeexp,evn,lbt::args),
			NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end

fun FEVENTexp (evn,lbt,args) =
     let val (ntimedec,ntimeexp) = makent "_newtime"
     in LETexp (ntimedec,
	        SEQexp [SETTIMEexp ntimeexp,
		        APPexp(VARexp (ref breakentry,NONE),
			       TUPLEexp(INTexp evn::lbt::args)),
	                NOTETIMEexp(ntimeexp,evn),
			ntimeexp])
     end

(* Because ref is a constructor that cannot be replaced at top level, 
    we replace it explicitly here: *)

val hcreater = makevar "_hcreater"
val HREFexp = VARexp(ref hcreater,NONE)

(* For efficiency, we implement

fun hass (objr,value) =
        (updatedRList := (weak objr) :: (!updatedList);
	 objr := value)
	
in-line. Note we maintain type information in opr used by translate to
determine whether to use boxed or unboxed update. *)

val weakvar = makevar "_weak"
val udrl = makevar "_udrl"
fun HASSIGNexp opr = 
     let val objvar = makenvar "_obj"
	 val valvar = makenvar "_val"
	 val objexp = VARexp(ref objvar,NONE)
         val valexp = VARexp(ref valvar,NONE)
      (* val newobj = APPexp(VARexp(ref weakvar,NONE),objexp)  *)
         val newobj = APPexp(VARexp(ref specialop,NONE), 
			   TUPLEexp[INTexp System.Tags.special_weak, objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop,NONE),VARexp(ref udrl,NONE))
     in FNexp([RULE(TUPLEpat[VARpat objvar,VARpat valvar],
	            SEQexp[APPexp(VARexp(ref opr,NONE), 
		                  TUPLEexp[objexp,valexp]),
			   APPexp(VARexp(ref assop,NONE),
	                          TUPLEexp[VARexp(ref udrl,NONE),
			                   APPexp(CONexp(consDcon,NONE),
				           TUPLEexp[newobj,oldlist])])]),
	       RULE(WILDpat,INTexp 0)],UNDEFty)   (* UNDEFty may not be
                                                   * appropriate here. (zsh)
                                                   *)
     end

val pconsvar = makevar "_pcons"
val udal = makevar "_udal"
fun HUPDATEexp opr =
     let val objvar = makenvar "_obj"
	 val offvar = makenvar "_off"
	 val valvar = makenvar "_val"
	 val objexp = VARexp(ref objvar,NONE)
	 val offexp = VARexp(ref offvar,NONE)
         val valexp = VARexp(ref valvar,NONE)
      (* val newobj = APPexp(VARexp(ref weakvar,NONE),objexp)  *)
         val newobj = APPexp(VARexp(ref specialop,NONE), 
			   TUPLEexp[INTexp System.Tags.special_weak, objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop,NONE),VARexp(ref udal,NONE))
     in FNexp([RULE(TUPLEpat[VARpat objvar,VARpat offvar, VARpat valvar],
	            SEQexp[APPexp(VARexp(ref opr,NONE), 
		                  TUPLEexp[objexp,offexp,valexp]),
			   APPexp(VARexp(ref assop,NONE),
	                          TUPLEexp[VARexp(ref udal,NONE),
			                   APPexp(VARexp(ref pconsvar,NONE),
						  TUPLEexp[newobj,offexp,oldlist])])]),
	       RULE(WILDpat,INTexp 0)],UNDEFty)   (* UNDEFty may not be
                                                   * appropriate here. (zsh)
                                                   *)
     end


fun simplebind ((btexp,bsites,bvars,bhasfn),ev,vars) =
       let val evn = makeEvn (ev::bsites)
	   val bevvar = makevar("_bind" ^ makestring evn)
	   val evndec = VALdec[VB{pat=VARpat(bevvar),
		                    exp=EVENTexp(evn,btexp,vars@bvars),
				    tyvars=ref nil}]
       in ((VARexp(ref bevvar,NONE),nil,nil,false),evndec)
       end

(* Variable naming conventions:
     btexp:exp  represents time of last binding event
     bsites:place list  places of accumulated events to be discharged
     bvars:exp list     associated accumulated variables 
     bhasfn:bool       bsites includes a FNev or HANDLEev *)

fun instrexp2(b as (btexp:exp,bsites:place list,bvars:exp list,bhasfn:bool),
	      exp:exp) : 
     exp (* instrumented expression *) =
  let 
    fun instr (RECORDexp l) =
	  let fun f(lab,exp) = (lab,instr exp)
	  in RECORDexp (map f l)
	  end
      | instr (VECTORexp l) = VECTORexp(map instr l)
      | instr (SEQexp expl) = SEQexp(map instr expl)
      | instr (CONexp(DATACON{rep=REF,...},_)) = HREFexp
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.ASSIGN),
                                           ...}),_)) =
		HASSIGNexp assopr
      | instr (VARexp(ref(updopr as VALvar{access=INLINE(P.UPDATE),
                                           ...}),_)) =
		HUPDATEexp updopr
      | instr (VARexp(ref(updopr as VALvar{access=INLINE(P.INLUPDATE),
                                           ...}),_)) =
		HUPDATEexp updopr
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val opr' = instr opr
		     val arg' = instr arg
		     val evn = makeEvn(makeEvent(APPev exp)::bsites)
		     val oprtmp = makenvar "_oprvar"
		     val argtmp = makenvar "_argvar"
		 in LETexp(VALdec[VB{pat = VARpat(oprtmp), 
				     exp = opr', 
				     tyvars =ref nil},
				  VB{pat = VARpat(argtmp), 
				     exp = arg', 
				     tyvars =ref nil}],
			   SEQexp [EVENTexp(evn,btexp,bvars), 
				   APPexp(VARexp(ref oprtmp,NONE), 
					  VARexp(ref argtmp,NONE))])
		 end
	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.ASSIGN),...}),_) => normal()
	     | VARexp(ref(VALvar{access=INLINE(P.UPDATE),...}),_) => normal()
	     | VARexp(ref(VALvar{access=INLINE(P.INLUPDATE),
                                 ...}),_) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}),_)) =>
		 let val arg' = instr arg
		 in APPexp(opr,arg')
                 end
	     | CONexp(DATACON{rep=REF,...},_) => normal()
	     | CONexp _ => APPexp(opr, instr arg)
             | FNexp(body,t) =>  (* really a CASE or equivalent *)
		 let val arg' = instr arg
		     val body' = 
			 instrrules (b,fn r => makeEvent(CASEev(arg,r))) body
		 in APPexp(FNexp(body',t),arg')
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) = 
	  let val e' = instr e
	  in CONSTRAINTexp(e',c)
          end
      | instr (exp as MARKexp(RAISEexp(arg,t),_,_)) =
		(* N.B. Must be marked *)
	  let val arg' = instr arg
	      val evn = makeEvn(makeEvent(RAISEev exp)::bsites)
	      val argtmp = makenvar "_argvar"
	  in LETexp (VALdec [VB{pat = VARpat(argtmp),
				exp = arg',
				tyvars = ref nil}],
		     SEQexp [EVENTexp(evn,btexp,bvars),
			     RAISEexp(VARexp(ref argtmp,NONE),t)])
	  end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
          let val ((btexp',bsites',bvars',bhasfn'),ldec') = instrdec (b,ldec)
	      val evn = makeEvn(makeEvent(LETev exp)::bsites')
	      val bevvar = makevar("_bind" ^ makestring evn)
	      val lexp' = instrexp2((VARexp(ref bevvar,NONE),
                                     nil,nil,false),lexp)
	  in LETexp(SEQdec[ldec',
			   VALdec[VB{pat=VARpat bevvar,
				     exp=EVENTexp(evn,btexp',bvars'),
				     tyvars=ref nil}]],
		    lexp')
          end
      | instr (CASEexp(exp,rl)) = 
	  let val exp' = instr exp
	      val rl' =  instrrules (b,fn r => makeEvent(CASEev(exp,r))) rl
	  in CASEexp(exp',rl')
	  end
      | instr (HANDLEexp (e, HANDLER(FNexp(body,t)))) =
     	    let val e' = instr e
     	        val body' = instrrules(b,makeEvent o HANDLEev) body
     	    in HANDLEexp(e',HANDLER(FNexp(body',t)))
            end
      | instr (FNexp(body,t)) = 
	    let val body' = instrrules (b,makeEvent o FNev) body
	    in FNexp(body',t)
	    end
      | instr (MARKexp (exp,s,e)) =
	    let val exp' = instr exp
	    in MARKexp(exp',s,e)
	    end
      | instr exp = exp
    and instrrules (b as (btexp,bsites,bvars,bhasfn),evf) = 
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v,NONE)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
		 val evn = makeEvn bsites'
		 val bevvar = makevar("_bind" ^ makestring evn)
		 val exp' = instrexp2((VARexp(ref bevvar,NONE),
                                       nil,nil,false),exp)
	     in RULE(pat, LETexp(VALdec[VB{pat=VARpat bevvar,
					   exp=EVENTexp(evn,btexp,bvars'),
					   tyvars=ref nil}],
				 exp'))
	     end
	  | f (RULE(pat,CONSTRAINTexp(exp,_))) =  f (RULE(pat,exp))
          | f (RULE(pat,exp)) =
	     let val exp' = instrexp2(b,exp)
	     in RULE(pat,exp')
             end
      in map f 
      end
  in 
    instr exp
  end

(* The idea of levels 3/4 is as follows: for every instrumented expression
   we return the flag (d:bool), which is set iff the expr discharges
   (any and all) events on the bsites list (even if that list is empty).
   If d is true, the instrumented expression returns a pair (lbt,value);
   otherwise it just returns the value.
   It is important that every event containing a FNev is executed immediately
   after the event containing the matching APPev.  This implies that each
   event must contain at most one FNev, and that each FNev is contained
   in at most one event in any possible path through the instrumented code.
   We arrange the stronger condition that, if there is a FNev to be 
   discharged, every expression either discharges it or executes no 
   events at all; this means that forcing FNevs at the top of rules 
   should never be necessary.
   We do this by forcing events before LETs, HANDLEs and CASEs where necessary.
   (Note that this means a FNev can never be pushed inside a CASE or LET,
   lest we need to discharge the FNev again in a subsequent event.  This
   is unnecessaary if there are no subsequent events, but our present
   analysis is too stupid to notice this.)
   We also force discharge of FNevs before FNs, to prevent the two FNs in
   one list problem.
   The remaining issue is testing for backstop events in LET bodies and
   FN/CASE rule bodies.  Clearly, if the discharge flag is set, no backstop is
   needed; but this is not a sufficient test because
   LETs, CASEs, and HANDLEs all execute events (so no backstop is necessary),
   but don't in general discharge. (Actually, it is sufficient for FN
   rule bodies, where there will always be a FNev to discharge.)
   Tentative Soln #1: Strengthen the forced discharge before LET,CASE, and 
   HANDLE to insert an event if there are any events *at all* in the
   bsites list.  If we're in the body of a  LET (of any conceivable interest) 
   or a CASE (or of course a FN) there will be such an event.
   Then backstop needed can be equated with d false. THIS is level #3
   Tentative Soln #2: Just allow the extra backstop events. This may be
   a winner on the whole. This is level #4.
*)
and instrexp3(b as (btexp:exp,bsites:place list,bvars:exp list,bhasfn:bool),
	      exp:exp) : bool * exp =
(* bool: d = true iff instrumented expression discharges (any and all) 
             events (even if none to discharge 
   exp: exp' = instrumented expression. Consists of a tuple (lbt,value)
                iff d = true, else just a value.
*)
  let
    fun discharge (allsites:bool,
		   f:(exp * place list * exp list * bool) -> exp) =
	(* Discharge any function events (any events at all if allsites true)
	   before executing expression constructed by f. *)
	  let val (btopt,b as (btexp,bsites,bvars,bhasfn)) =
	         if (allsites andalso (!instrumLevel = 3)
		     andalso (not (null bsites))) 
		   orelse bhasfn then
		   let val btvar = makenvar "_btvar"
		       val evn = makeEvn bsites
		       val btdec = 
			   VALdec[VB{pat=VARpat btvar,
				     exp=EVENTexp(evn,btexp,bvars),
				     tyvars=ref nil}]
		   in (SOME btdec,(VARexp(ref btvar,NONE),nil,nil,false))
		   end
		 else (NONE,b)
	      val exp' = f b
          in case btopt of
	       SOME btdec =>
		 (true,LETexp(btdec,TUPLEexp[btexp,exp']))   (* !! *)
	     | NONE => (false,exp')
          end
    fun instr (RECORDexp l) = 
          let fun f ((lab as LABEL{name,...},exp)::rest,accv,accd) = 
 	          let val fieldtmp =
		              makevar("_field:" ^ Symbol.name name)
		  in f (rest,
		        (lab,VARexp(ref fieldtmp,NONE)) :: accv,
			(fieldtmp,exp) :: accd)
		  end
	        | f (nil,accv,accd) = (rev accv, rev accd)
              val (lv,ld) = f (l,nil,nil)
		  (* lv is list of (label,varexp) pairs
		     ld is list of (var,exp) pairs *)
	      fun g (b as (btexp,bsites,bvars,bhasfn),(fieldtmp,exp)::rest) = 
		    let val (d,exp') = instrexp3(b,exp)
		    in if d then
			 let val btvar = makenvar "_btvar"
		             val dec' = VALdec[VB{pat=TUPLEpat[VARpat btvar,
							       VARpat fieldtmp],
						  exp=exp',
						  tyvars=ref nil}]
			     val (_,rest') = 
				 g((VARexp(ref btvar,NONE),nil,nil,false),rest)
			 in (SOME(VARexp(ref btvar,NONE)),dec'::rest')
                         end
		       else
			 let val dec' = VALdec[VB{pat=VARpat fieldtmp,
						  exp=exp',
						  tyvars=ref nil}]
			     val (btopt,rest') = g(b,rest)
			 in (btopt,dec'::rest')
			 end
		    end
                | g (_,nil) = (NONE,nil)
	      val (btopt,ld') = g(b,ld)
	      (* avoid decl's when possible, for cosmetic reasons *)
	      fun h ((lab,_)::rl,(VALdec[VB{exp,...}])::re) = 
		         (lab,exp)::(h(rl,re))
		| h (nil,nil) = nil
		| h _ = impossible "DebugInstrum.instrexp RECexp"
	  in case btopt of
	       SOME (btexp') => (true,
				 LETexp(SEQdec ld',
					TUPLEexp[btexp',
						 RECORDexp lv]))  (* !! *)
	     | NONE => (false, RECORDexp(h(lv,ld')))
	  end 
      | instr (VECTORexp expl) = 
	 (* NEED SOMETHING HERE *)
	 debugPanic "Can't use instrumLevel 3/4 with VECTORexp"
      | instr (SEQexp expl) =
	  let 
	    fun g(usebt,b as (btexp,bsites,bvars,bhasfn),exp::rest) =
		  let val (d,exp') = instrexp3(b,exp)
		  in if d then
		       let val btvar = makenvar "_btvar"
			   val valvar = makenvar "_valvar"
			   val (_,rest') = 
			       g(false,(VARexp(ref btvar,NONE),
                                        nil,nil,false),rest)
			   val useexp = 
			      if usebt then
				TUPLEexp[VARexp(ref btvar,NONE),
					 SEQexp(VARexp(ref valvar,NONE)::
						rest')]
			      else SEQexp(VARexp(ref valvar,NONE)::rest')
		       in (true,
			   [LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							  VARpat valvar],
					    exp=exp',
					    tyvars=ref nil}],
				   useexp)])
		       end
		     else 
		       let val (d',rest') = g(usebt,b,rest)
		       in (d',exp'::rest')
		       end
		  end
	      | g(_,_,nil) = (false,nil)
	    val (d,expl') = g(true,b,expl)
          in (d,SEQexp expl')                          (* !! *)
          end
      | instr (CONexp(DATACON{rep=REF,...},_)) = (false,HREFexp)
      | instr (VARexp(ref(assopr as VALvar{access=INLINE(P.ASSIGN),
                                           ...}),_)) =
	        (false,HASSIGNexp assopr)
      | instr (VARexp(ref(updopr as VALvar{access=INLINE(P.UPDATE),
                                           ...}),_)) =
		(false,HUPDATEexp updopr)
      | instr (VARexp(ref(updopr as VALvar{access=INLINE(P.INLUPDATE),
                                           ...}),_)) =
		(false,HUPDATEexp updopr)
      | instr (exp as APPexp(opr,arg)) =
	  let fun strip (MARKexp(exp,_,_)) = strip exp
		| strip (CONSTRAINTexp(exp,_)) = strip exp
		| strip (SEQexp[exp]) = strip exp
		| strip exp = exp
	      fun normal () =
		 let val oprtmp = makenvar "_oprvar"
		     val argtmp = makenvar "_argvar"
		     val btvar = makenvar "_btvar"
		     val (dopr,opr') = instr opr
		     val oprpat =
			if dopr then
			  TUPLEpat[VARpat btvar,VARpat oprtmp]
			else VARpat oprtmp
		     val (darg,arg') =
		       if dopr then
			 instrexp3((VARexp(ref btvar,NONE),nil,nil,false),arg)
		       else instr arg
		     val argpat = 
		       if darg then
                         if dopr then
			   TUPLEpat[WILDpat, VARpat argtmp]
			 else TUPLEpat[VARpat btvar,VARpat argtmp]
		       else VARpat argtmp
		     val appexp = APPexp(VARexp(ref oprtmp,NONE),
					 VARexp(ref argtmp,NONE))
		     val fullexp =
		        if dopr orelse darg then 
			  let val evn = makeEvn[makeEvent(APPev exp)]
			  in TUPLEexp[VARexp(ref btvar,NONE),
				      SEQexp[EVENTexp(evn,VARexp(ref btvar,
                                                                 NONE),nil),
					     appexp]]
			  end
			else 
			  let val evn = 
			        makeEvn(makeEvent(APPev exp)::bsites)
			  in TUPLEexp[EVENTexp(evn,btexp,bvars),
				      appexp]
			  end
		 in (true,
		     LETexp(SEQdec[VALdec[VB{pat = oprpat,
					     exp = opr', 
					     tyvars = ref nil}],
				   VALdec[VB{pat = argpat,
					     exp = arg', 
					     tyvars = ref nil}]],
			    fullexp))
		 end
 	  in case (strip opr) of
	       VARexp(ref(VALvar{access=INLINE(P.ASSIGN),...}),_) => normal()
	     | VARexp(ref(VALvar{access=INLINE(P.UPDATE),...}),_) => normal()
	     | VARexp(ref(VALvar{access=INLINE(P.INLUPDATE),
                                 ...}),_) => normal()
	     | (opr as VARexp(ref(VALvar{access=INLINE prim,...}),_)) =>
		 let val (darg,arg') = instr arg
		     val argtmp = makenvar "_argvar"
		 in if darg then
		      let val btvar = makenvar "_btvar"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=ref nil}],
				      TUPLEexp[VARexp(ref btvar,NONE),
					       APPexp(opr,
						  VARexp(ref argtmp,NONE))]))
		      end
		    else if bhasfn andalso (Prim.mayRaise prim) then
		      let val evn = makeEvn(makeEvent(APPev exp)::bsites)
		      in (true,LETexp(VALdec[VB{pat=VARpat argtmp,
						exp=arg',
						tyvars=ref nil}],
				      TUPLEexp[EVENTexp(evn,btexp,bvars),
					       APPexp(opr,
						  VARexp(ref argtmp,NONE))]))
		      end
  		    else (false,APPexp(opr,arg'))
		 end
	     | CONexp(DATACON{rep=REF,...},_) => normal()
	     | CONexp _ => 
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val btvar = makenvar "_btvar"
			  val argtmp = makenvar "_argtmp"
		      in (true,LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							     VARpat argtmp],
						exp=arg',
						tyvars=ref nil}],
				      TUPLEexp[VARexp(ref btvar,NONE),
					       APPexp(opr,
						  VARexp(ref argtmp,NONE))]))
		      end
		    else (false,APPexp(opr,arg'))
		 end
             | FNexp(body,t) =>  (* really a CASE or equivalent *)
		 let val (darg,arg') = instr arg
		 in if darg then
		      let val valvar = makenvar "_valtmp"
			  val btvar = makenvar "_btvar"
			  val body' = 
			      instrrules((VARexp(ref btvar,NONE),
                                          nil,nil,false),
					 fn r => makeEvent(CASEev(arg,r)),
					 false) body
		      in (true,
			  LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
							VARpat valvar],
					   exp=arg',
					   tyvars=ref nil}],
				 TUPLEexp[VARexp(ref btvar,NONE),
					  APPexp(FNexp(body',t),
						 VARexp(ref valvar,NONE))]))
		      end
		    else 
		      discharge (true,fn b =>
		        let val body' = 
			    instrrules(b,
				       fn r => makeEvent(CASEev(arg,r)),
				       false) body
			in APPexp(FNexp(body',t),arg')
			end)
		 end
	     | _ => normal()
	  end
      | instr (CONSTRAINTexp (e,c)) =
	  let val (d,e') = instr e
          in (d,CONSTRAINTexp(e',c))
	  end
      | instr (exp as MARKexp(RAISEexp (arg,t),_,_)) = 
		(* N.B. Must be marked *)
	  let val argtmp = makenvar "_argvar"
	      val (darg,arg') = instr arg
	  in if darg then
	       let val evn = makeEvn[makeEvent(RAISEev exp)]
		   val btvar = makenvar "_btvar"
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,
						 VARpat argtmp],
				    exp=arg',
				    tyvars=ref nil}],
			  TUPLEexp[VARexp(ref btvar,NONE),
			           SEQexp[EVENTexp(evn,VARexp(ref btvar,NONE),nil),
					  RAISEexp(VARexp(ref argtmp,NONE),t)]]))
               end
             else 
	       let val evn = makeEvn(makeEvent(RAISEev exp)::bsites)
               in (true,
		   LETexp(VALdec[VB{pat=VARpat argtmp,
			 	    exp=arg',
				    tyvars=ref nil}],
			  TUPLEexp[EVENTexp(evn,btexp,bvars),
				   RAISEexp(VARexp(ref argtmp,NONE),t)]))
	       end
	   end
      | instr (exp as MARKexp(LETexp (ldec,lexp),_,_)) = 
		(* note: must be marked *)
	  (* We discharge any events before executing LET. *)
	  discharge (true,fn (b as (btexp,bsites,bvars,bhasfn)) =>
	      (* assume instrdec returns a btexp' valid in context of ldec' *)
	    let val ((btexp',bsites',bvars',bhasfn'),ldec') =
		           instrdec ((btexp,bsites,bvars,bhasfn),ldec)
		val (d',lexp') = instrexp3((btexp',bsites',bvars',bhasfn'),
					   lexp)
	    in if d' then
		   let val valvar = makenvar "_valtmp"
		   in LETexp(ldec',
			     LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							   VARpat valvar],
					      exp=lexp',
					      tyvars=ref nil}],
				    VARexp (ref valvar,NONE)))
		   end
	       else (* If anything at all was bound by ldec, then if 
		       any event is executed in body it would have discharged.
		       So at this point, we need a backstop event. *)
		   let val evn = makeEvn(makeEvent(LETev exp)::bsites')
		   in LETexp(SEQdec[ldec',
				    VALdec[VB{pat=WILDpat,
					      exp=EVENTexp(evn,btexp',bvars'),
					      tyvars=ref nil}]],
			     lexp')
		   end
	    end)
      | instr (CASEexp(exp,rl)) = 
	  let val (d,exp') = instr exp
	  in if d then
	       let val valvar = makenvar "_valtmp"
		   val btvar = makenvar "_btvar"
		   val rl' = instrrules((VARexp(ref btvar,NONE),nil,nil,false),
					fn r => makeEvent(CASEev(exp,r)),
				        false) rl
	       in (true,
		   LETexp(VALdec[VB{pat=TUPLEpat[VARpat btvar,VARpat valvar],
				    exp=exp',
				    tyvars=ref nil}],
			  TUPLEexp[VARexp(ref btvar,NONE),
				   CASEexp(VARexp(ref valvar,NONE),
					   rl')]))
	       end
	     else 
               discharge (true,fn b =>		 
		 let val rl' = 
			instrrules(b,fn r => makeEvent(CASEev(exp,r)),false) rl
		 in CASEexp(exp',rl')
		 end)
	  end
	
      | instr (HANDLEexp (e, HANDLER(FNexp(body,t)))) =
	  (* We discharge any events before executing HANDLEexp *)
	  discharge (true,fn b => 
	    let val (d',e') = instrexp3(b,e)
		val e' = 
		    if d' then
		      let val valvar = makenvar "_valtmp"
		      in LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,VARpat valvar],
					  exp=e',
					  tyvars=ref nil}],
				VARexp (ref valvar,NONE))
		      end
		    else e'
		val body' = instrrules(b,makeEvent o HANDLEev,true) body
	    in HANDLEexp(e',HANDLER(FNexp(body',t)))
	    end)
      | instr (FNexp(body,t)) =
	  (* We discharge any function events before executing FNexp *)
	  discharge (false,fn b =>
	    let val body' = instrrules(b,makeEvent o FNev,true) body
	    in FNexp(body',t)
	    end)
      | instr (MARKexp (exp,s,e)) =
          let val (d,exp') = instr exp
          in (d, MARKexp(exp',s,e))
          end
      | instr exp = (false,exp)
    and instrrules (b as (btexp,bsites,bvars,bhasfn),evf,addsfn) =
	(* always return just a value *)
      let 
        fun f (rule as RULE(pat,exp as MARKexp(_))) = 
	     let val vars = (patvars (fn v => VARexp(ref v,NONE)) pat)
		 val bsites' = (evf rule)::bsites
		 val bvars' = vars@bvars
		 val bhasfn' = bhasfn orelse addsfn
		 val (d,exp') = instrexp3((btexp,bsites',bvars',bhasfn'),exp)
	     in if d then
		  let val valvar = makenvar "_valtmp"
		  in RULE(pat,
			  LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							VARpat valvar],
					   exp=exp',
					   tyvars=ref nil}],
				 VARexp(ref valvar,NONE)))
		  end
		else (* need backstop *)
		  let val evn = makeEvn bsites'
		  in RULE(pat,
			  SEQexp[EVENTexp(evn,btexp,bvars'),
				 exp'])
		  end
	      end
	  | f (RULE(pat,CONSTRAINTexp(exp,_))) =  f (RULE(pat,exp))
          | f (RULE(pat,exp)) =
	     let val (d,exp') = instrexp3(b,exp)
	     in if d then
 	          let val valvar = makenvar "_valtmp"
		  in RULE(pat,
			  LETexp(VALdec[VB{pat=TUPLEpat[WILDpat,
							VARpat valvar],
					   exp=exp',
					   tyvars=ref nil}],
				 VARexp(ref valvar,NONE)))
		  end
		else RULE(pat,exp')
             end
      in map f 
      end
  in
    instr exp
  end



and instrdec (b as (btexp:exp, bsites:place list,bvars: exp list,bhasfn:bool),
	     dec:dec) : 
	((exp * (place list) * (exp list) * bool) * dec) =
  let
    fun instr (dec as MARKdec(VALdec vbl,_,_)) =
          if !instrumLevel = 2 then
	    let val vars = vblextract (fn v => VARexp(ref v,NONE)) vbl
		fun f (VB{pat,exp,tyvars}) =
		    VB{pat=pat,exp=instrexp2(b,exp),tyvars=tyvars}
	    in ((btexp,makeEvent(VALev dec)::bsites,vars@bvars,bhasfn),
		VALdec(map f vbl))
	    end
          else (* !instrumLevel = 3 or 4 *)
	    let val vars = vblextract (fn v => VARexp(ref v,NONE)) vbl
		fun g(VB{pat,exp,tyvars}::rest) =
		     let val (d,exp') = 
			 instrexp3((btexp,bsites,bvars,bhasfn),exp)
		     in if d then
			  let val btvar = makenvar "_btvar"
			      val dec' =
			         VALdec[VB{pat=TUPLEpat[VARpat btvar,
							pat],
					   exp=exp',
					   tyvars=ref nil}]
			      val (_,rest') = g rest 
			  in (SOME(VARexp(ref btvar,NONE)),dec'::rest')
			  end
			else
			  let val dec' = VALdec[VB{pat=pat,
						   exp=exp',
						   tyvars=ref nil}]
			      val (btopt,rest') = g rest
			  in (btopt,dec'::rest')
			  end
                     end
		  | g nil = (NONE,nil)
	       val (btopt,decl') = g vbl
	       val (btexp',bsites',bvars',bhasfn') =
		  case btopt of
		    SOME btexp' => (btexp',nil,nil,false)
		  | NONE => (btexp,bsites,bvars,bhasfn)
	    in ((btexp',makeEvent(VALev dec)::bsites',vars@bvars',bhasfn'),
		SEQdec decl')
	    end
      | instr (dec as MARKdec(VALRECdec rvbl,_,_)) =
            (* N.B. Bodies cannot discharge vars ! *)
	  if !instrumLevel = 2 then
	    let val vars = map (fn RVB{var,...} => VARexp(ref var,NONE)) rvbl
		val b' = (btexp,makeEvent(VALRECev dec)::bsites,
			  vars @ bvars,bhasfn)
		fun g (RVB{var,exp,resultty,tyvars}) =
		     let val exp' = instrexp2(b',exp)
		     in RVB {var=var,
			     exp=exp',
			     resultty=resultty,
			     tyvars=tyvars}
		     end
		val rvbl' = map g rvbl
	    in (b',VALRECdec rvbl')
	    end
	  else (* instrumLevel = 3 or 4 *)
	    let val vars = map (fn RVB{var,...} => VARexp(ref var,NONE)) rvbl
		val bsites' = makeEvent(VALRECev dec)::bsites
		val bvars' = vars@bvars
		fun g (RVB{var,exp,resultty,tyvars}) =
		     let val (d,exp') = 
			   instrexp3((btexp,bsites',bvars',bhasfn),exp)
		     in if d then debugPanic "instrDec VALRECdec"
			else RVB{var=var,
				 exp=exp',
				 resultty=resultty,
				 tyvars=tyvars}
		     end
                val rvbl' = map g rvbl
	    in ((btexp,bsites',bvars',bhasfn),VALRECdec rvbl')
	    end
      | instr (SEQdec decl) =
	    let val (b', decl') = instrlist (b, decl)
	    in (b', SEQdec decl')
	    end
      | instr (dec as MARKdec(LOCALdec(localdec, visibledec),_,_)) = 
	    let val localEv = makeEvent(LOCALev dec)
		val (b' as (btexp',bsites',bvars',bhasfn'), localdec') = 
		   instrdec ((btexp,localEv::bsites,bvars,bhasfn),
			     localdec)
		val locinEv = makeEvent(LOCINev dec)
		val (b'' as (btexp'',bsites'',bvars'',bhasfn''), visibledec') =
		   instrdec ((btexp',locinEv::bsites',bvars',bhasfn'),
			     visibledec)
	    in if exists (fn x => x = locinEv) bsites'' andalso 
		  not (exists (fn x => x = localEv) bsites'') then
		   (* there is a local evn but no visible evn;
		      must manufacture a visible evn to expose 
                      local evn. *)
		 let val (b'',locenddec) = 
		           simplebind(b'',makeEvent(LOCENDev dec),nil)
		 in (b'',LOCALdec(localdec',SEQdec[visibledec',locenddec]))
		 end
	       else 
		 ((btexp'',makeEvent(LOCENDev dec)::bsites'',bvars'',bhasfn''),
	          LOCALdec(localdec',visibledec'))
	    end
      | instr (dec as MARKdec(ABSTYPEdec {abstycs,withtycs,body},_,_)) =
	    let val ((btexp',bsites',bvars',bhasfn'), body') = instr body
	    in ((btexp',makeEvent(TYPEev dec)::bsites',bvars',bhasfn'),
		ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,body=body'})
	    end
      | instr (dec as MARKdec(STRdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (strbl',lasttimedec,strvl) = 
		    instrstrbl timearrexp strbl
		val dec' = LOCALdec (timearrdec, 
				     SEQdec[STRdec strbl',lasttimedec])
	    in ((btexp,
                 makeEvent(STRev dec)::bsites,
		 timearrexp::(strvl @ bvars),
		 bhasfn), 
                dec')
	    end
      | instr (dec as MARKdec(ABSdec strbl,_,_)) =
            let val (timearrdec,timearrexp) = maketimearr (length strbl)
	        val (strbl',lasttimedec,strvl) = 
		    instrstrbl timearrexp strbl
		val dec' = LOCALdec (timearrdec, 
				     SEQdec[ABSdec strbl',lasttimedec])
	    in ((btexp,
                 makeEvent(ABSev dec)::bsites,
		 timearrexp::(strvl @ bvars),
		 bhasfn), 
                dec')
	    end
      | instr (dec as MARKdec(FCTdec fctbl,_,_)) =
	    let 
              fun instrfctb (fctb as FCTB{fctvar,def=FCTfct{param,def,thin,
							    constraint}}) =
                let val (b',entdec) = 
		         simplebind(b,makeEvent(FCTENTev fctb),nil)
                    val def' = instrstrexp(b',def)
                in FCTB{fctvar=fctvar,
			def=FCTfct{param=param,
				   def=LETstr(entdec,def'),
				   thin=thin,constraint=constraint}}
                end
		| instrfctb fctb = fctb
	      fun tovar (FCTB{fctvar=FCTvar{name,access=PATH[lv],...},...}) =
		    fakeuse(name,lv)
	      val fctvl = map tovar fctbl
	      val fctbl' = map instrfctb fctbl
	    in ((btexp,makeEvent(FCTev dec)::bsites,fctvl @ bvars,bhasfn),
		FCTdec fctbl')
            end
      | instr (dec as MARKdec(SIGdec _,_,_)) =
	    ((btexp,makeEvent(SIGev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec(FSIGdec _,_,_)) =
	    ((btexp,makeEvent(FSIGev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec(TYPEdec _,_,_)) =
            ((btexp,makeEvent(TYPEev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec(FIXdec _,_,_)) =
	    ((btexp,makeEvent(FIXev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec (OVLDdec _,_,_)) =
	    ((btexp,makeEvent(OVLDev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec(DATATYPEdec _,_,_)) =
            ((btexp,makeEvent(TYPEev dec)::bsites,bvars,bhasfn),dec)
      | instr (dec as MARKdec(EXCEPTIONdec ebl,_,_)) =
	    let fun extract (EBgen{exn,...}) = exn
		  | extract (EBdef{exn,...}) = exn
	        fun tovar (DATACON{name,rep=VARIABLE(PATH [lv]),...}) =
		      fakeuse(name,lv)
		  | tovar (DATACON{name,rep=VARIABLEc(PATH [lv]),...}) =
		      fakeuse(name,lv)
		  | tovar _ = debugPanic "instrDec EXCEPTIONdec"
	        val ebreps = map (tovar o extract) ebl
		fun convert (EBgen{exn,etype,ident}) =
		       EBgen{exn=exn,etype=etype,
			     ident=TUPLEexp[ident,INCexp(GETTIMEexp)]}
		  | convert ebdef = ebdef
		val ebl' = map convert ebl
		val (b',exndec) = 
		    simplebind(b,makeEvent(EXCEPTIONev dec),ebreps)
	    in (b',SEQdec[EXCEPTIONdec ebl',exndec])
	    end
      | instr (dec as MARKdec(OPENdec _,_,_)) =
	    ((btexp,makeEvent(OPENev dec)::bsites,bvars,bhasfn),dec)
      | instr (MARKdec (dec,s,e)) = 
	    let val (b',dec') = instr dec
	    in (b', MARKdec (dec',s,e))
	    end
      | instr dec = (b, dec)

   and instrstrbl timearrexp strbl = 
         let 
           fun dostrb (STRB{strvar,def,thin,constraint},
		       (n,lasttimedec,strbl))= 
	        let val def' = instrstrexp(b,def)
		    val strb' = STRB{strvar=strvar,
				     def=LETstr(lasttimedec,def'),
				     thin=thin,
				     constraint=constraint}
		    val lasttimedec' = 
			VALdec[VB{pat=WILDpat,
				  exp=APPexp(VARexp(ref updateop,NONE),
					     TUPLEexp[timearrexp,
						      INTexp n,
						      GETTIMEexp]),
				  tyvars=ref nil}]
		in (n+1,
		    lasttimedec',
	            strb' :: strbl)
		end
	   val (_,lasttimedec,strbl') = 
	       revfold dostrb strbl (0,SEQdec nil,[])
  	   fun tovar (STRB{strvar=STRvar{name=n,access=PATH [lv],...},...}) =
		 fakeuse(n,lv)
	   val strvl = map tovar strbl
	 in (rev strbl',lasttimedec,strvl)
         end
    in instr dec
   end

and instrstrexp (b,mstrexp as MARKstr(strexp,_,_)) =
     (case strexp of 
        STRUCTstr{body,locations,str} =>
          let val (b',body') = instrlist(b,body)
              val (_,enddec) = 
       	       simplebind(b',makeEvent(STRENDev mstrexp),nil)
          in STRUCTstr{body=body'@[enddec],locations=locations,
		       str=str}
          end
      | APPstr{oper,argexp,argthin,str} => 
          let val argexp' = instrstrexp (b,argexp)
	      val strvar = makenstrvar "_AnonStruct"
	      val strb = STRB{strvar=strvar,def=argexp',
			      thin=argthin (*??*),constraint=NONE}
	      val STRvar{access=PATH[lv],...} = strvar
	      val paramv = fakeuse (Symbol.varSymbol "param",lv)
              val (_,appdec) = 
       		simplebind(b,makeEvent(FCTAPPev mstrexp),[paramv])
          in LETstr(SEQdec[STRdec[strb],appdec], 
		    APPstr{oper=oper,argexp=VARstr strvar,
			   argthin=NONE (*??*),str=str})
          end
      | LETstr(dec,strexp) =>
          let val (b',dec') = instrdec(b,dec)
              val strexp' = instrstrexp(b',strexp)
          in LETstr(dec',strexp')
          end
      | VARstr _ =>
          let val (_,defdec) = 
       		simplebind(b,makeEvent(STRVARev mstrexp),nil)
          in LETstr(defdec,strexp)
          end
      | MARKstr _ => debugPanic "instrstrexp: double MARKstr")
  | instrstrexp (_)= debugPanic "instrstrexp: unmarked strexp"

and instrlist (b, decl) = 
   let fun g (dec::rest, b, acc) =
     		let val (b',dec') = instrdec(b,dec)
	        in g(rest,b',dec' :: acc)
	        end
	 | g (nil,b,acc) = (b,rev acc)
   in g (decl,b,nil)
   end

val absyn' =
  let val startevn = makeEvn [makeEvent(STARTev absyn)]
      val bevvar = makevar ("_bind" ^ makestring startevn)
      val STARTexp = EVENTexp(startevn,INTexp lastBindTime,nil)
      val STARTdec = VALdec[VB{pat=VARpat(bevvar),exp=STARTexp,tyvars=ref nil}]
      val ((btexp,bsites,bvars,bhasfn),absyn') = 
 	     instrdec((VARexp (ref bevvar,NONE),nil,nil,false),absyn)
      val endevn = makeEvn(makeEvent(ENDev absyn)::bsites)
      val ENDexp = FEVENTexp(endevn,btexp,VARexp(ref bevvar,NONE)::bvars)
      val ENDdec = VALdec[VB{pat=WILDpat,exp=ENDexp,tyvars=ref nil}]
  in  SEQdec[STARTdec,
	     absyn',
	     ENDdec]
  end


in 
  {absyn=LOCALdec(VALdec [VB {pat = TUPLEpat [VARpat timesvar,
					      VARpat eventtimes,
					      VARpat breakentry,
					      VARpat hcreater,
					      VARpat weakvar,
					      VARpat udrl,
					      VARpat pconsvar,
					      VARpat udal,
					      VARpat arrayvar],
                              exp = APPexp(VARexp(CoreInfo.getDebugVar,NONE),
					   INTexp (firstPlace)),
  	                      tyvars = ref nil}],
                  absyn'),
   events = vector(rev(!eventList)),
   evns = vector(rev(!evnList))}
end  (* fun instrumDec *)

end  (* structure debugInstrum *)
