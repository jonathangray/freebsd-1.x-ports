(* DebugBindings

   Synthesize symbol table bindings.
   Used by DebugEnv and DebugQueries.  
   
*)

signature DEBUG_BINDINGS =
sig
  type time
  type evn
  type evindex
  (* Basic binding finders. 
     Argument: qualified identifier (as string list)
               * time and evindex from which to start searching back.
     Returns: time,evindex of event where binding is found
              * (which element in event * binding itself)
     Raises: Env.Unbound if not successful. 
  *)
  val findVARCONBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findSTRBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findFCTBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findSIGBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findFSIGBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findTYCBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  val findFIXBind: (string list * time * evindex) -> 
                            (time * evindex * (int * Modules.binding))
  (* Drivers for calctype functions. *)
  val dynTypeAt: (time * evindex) -> Types.ty -> Types.ty
  val dynTyconAt: time -> Types.tycon -> Types.tycon
  val dynStrAt: time -> Modules.Structure -> Modules.Structure
  val pred : time-> time  (* Returns event executed just before time t,
			     omitting actions. *)
  val nthArgs: evn * evindex * System.Unsafe.object list ->
                  System.Unsafe.object list
	(* Return subset of args for given evindex of evn. *)
end

structure DebugBindings : DEBUG_BINDINGS =
struct
  (* Policy on interrupt: pass on QueryInterrupted. *)
  open Array List DebugUtil DebugStatic DebugRun Variables
       Access Absyn PrintUtil Modules Types
       BasicTypes DebugMod DynType
  structure U = System.Unsafe
  infix 9 sub

  val say = System.Print.say
  val eventsAt = eventsFor o evnAt

  fun pred (t:time) = 
    case hd(eventsAt (t-1)) of
      ENDev (_) => pred((U.cast (hd (argsAt(t-1)))):time)
    | SIGENDev => pred((U.cast (hd (argsAt(t-1)))):time)
    | _ => t-1
  
  (* Extract argument structure from argexp field of an APPstr. *)
  fun getArgStruct (argexp:strexp) =
      case argexp of
	VARstr (STRvar{binding,...}) => binding
      | STRUCTstr {str,...} => str
      | APPstr {str,...} => str
      | LETstr(_,strexp) => getArgStruct strexp
      | MARKstr(strexp,_,_) => getArgStruct strexp

  fun encfct t () = 
    let fun ef 0 = NOFCTCONTEXT
	  | ef t =
  	     let val (evn,lbt) = evnLbtAt t
	     in case eventsFor evn of
		 (FCTENTev(_)::_) =>
		   (case eventsAt (pred t) of
		      ((FCTAPPev(MARKstr(s as APPstr{oper,str,...},_,_)))::_)
		         => FCTCONTEXT{fct=oper,
				       str=str,
				       next_context = encfct (pred t)}
		    | _ => debugPanic "Bindings.encfct missing FCTAPPev")
		| _ => ef lbt
	     end
    in ef t
    end
  
  fun dynTyconAt t = deabstyc (encfct t)
  
  fun dynStrAt t = deabsstr (encfct t)
  
  fun dynTypeAt (t,c) (ty:ty) =
    let fun patTy pat =
	  case pat of
	    WILDpat => UNDEFty
	  | VARpat(VALvar{typ=ref ty,...}) => ty
	  | INTpat _ => intTy
          | REALpat _ => realTy
	  | STRINGpat _ => stringTy
          | CONSTRAINTpat(_,ty) => ty
	  | _ => debugPanic "Bindings.dynTypeAt.patTy bad pat"
        fun expTy exp =
          case exp of
            INTexp _ => intTy
          | REALexp _ => realTy
          | STRINGexp _ => stringTy
          | CONSTRAINTexp(_,ty) => ty
          | MARKexp(exp,_,_) => expTy exp
          | _ => debugPanic "Bindings.dynTypeAt.expTy bad exp"
	fun encfn (t,c) () =
          let fun ef (0,_) = NOFNCONTEXT
		| ef (t,c) =
		   let val (evn,lbt) = evnLbtAt t
		       fun f (nil) = ef (lbt,0)
			 | f (FNev(RULE(pat,_))::_) = 
               		         FNCONTEXT(patTy pat,
					   encfn(lbt,0),
					   argf(pred t))
			 | f (_::rest) = f rest
		   in f (tln(eventsFor evn,c))
		   end
	  in ef (t,c)
	  end
	and argf t () = 
          case hd(eventsAt t) of
	    APPev(APPexp(_,exp)) => FNARG(expTy exp,
					  encfn (t,0),dynTyconAt t)
	  | _ => NOFNARG  (* error ?? *)
	val _ = if (!debugdebug) 
		then PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
		       (fn ppstrm =>
			(PrettyPrint.add_string ppstrm "*statTy:"; 
			 PPType.resetPPType();
			 PPType.ppType (!debugStatEnv) ppstrm ty;
			 PrettyPrint.add_newline ppstrm))
		else ();
	val typ = dynType(ty, encfn(t,c), dynTyconAt t)
     in if (!debugdebug) then 
	  PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
	   (fn ppstrm =>
	    (PrettyPrint.add_string ppstrm "*dynTy:"; 
	     PPType.resetPPType();
	     PPType.ppType (!debugStatEnv) ppstrm typ;
	     PrettyPrint.add_newline ppstrm))
	else ();
	typ
    end
  
  exception Unbound = Env.Unbound
  
  (* find a binding within a structure *)
  fun findInStruct0 symbol (n:string, SIMPLE{env,...}:Structure) : binding =
          Env.look (env,symbol n)
    | findInStruct0 symbol (n:string, INSTANCE{sign=SIG{env,...},...}) = 
          Env.look (!env,symbol n)
    | findInStruct0 _ _ = debugPanic "Bindings.findInStruct bad Structure"
  
  val findVARCONInStruct = findInStruct0 Symbol.varSymbol
  val findSTRInStruct = findInStruct0 Symbol.strSymbol
  val findTYCInStruct = findInStruct0 Symbol.tycSymbol
  
  (* getting all bindings of a given sort at a particular event *)
  fun getSTRs ev: (structureVar list) =
       case ev of
	 STRev (MARKdec(STRdec strbl,_,_)) => 
		  map (fn STRB{strvar,...} => strvar) strbl
       | ABSev (MARKdec(ABSdec strbl,_,_)) => 
		  map (fn STRB{strvar,...} => strvar) strbl
       | _ => []
  
  fun getFCTs ev : (functorVar list) =
       case ev of
	 FCTev (MARKdec(FCTdec fctbl,_,_)) => 
		  map (fn FCTB{fctvar,...} => fctvar) fctbl
       | _ => []
  
  fun getSIGs ev : (signatureVar list) =
       case ev of
	 SIGev (MARKdec(SIGdec svl,_,_)) => svl
       | _ => []
  
  fun getFSIGs ev : (funsigVar list) =
       case ev of
	 FSIGev (MARKdec(FSIGdec fsvl,_,_)) => fsvl
       | _ => []

  fun getFIXs ev : (fixityVar list) = 
       case ev of
	 FIXev (MARKdec(FIXdec {fixity,ops},_,_)) =>
		map (fn name => FIXvar{name=name,binding=fixity}) ops
       | _ => []
  
  
  fun getTYCs ev : tycon list =
	case ev of
	  TYPEev(MARKdec(TYPEdec tbl,_,_)) => map (fn TB{tyc,...} => tyc) tbl
	| TYPEev(MARKdec(DATATYPEdec {datatycs=tycl,withtycs=tbl},_,_)) => 
				  tycl @ (map (fn TB{tyc,...} => tyc) tbl)
	| TYPEev(MARKdec(ABSTYPEdec {abstycs=tycl,withtycs=tbl,...},_,_)) =>
				  tycl @ (map (fn TB{tyc,...} => tyc) tbl)
	| _ => []
  
  fun getVARs ev : var list = 
	case ev of
	  VALev(MARKdec(VALdec(vbl),_,_)) => vblextract (fn x => x) vbl
	| VALRECev(MARKdec(VALRECdec(rvbl),_,_)) =>
				  map (fn RVB{var,...} => var) rvbl
	| FNev(RULE(pat,_)) => patvars (fn x => x) pat
	| HANDLEev(RULE(pat,_)) => patvars (fn x => x) pat
	| CASEev(_,RULE(pat,_)) => patvars (fn x => x) pat
	| OVLDev(MARKdec(OVLDdec ovldvar,_,_)) => [ovldvar]
	| _ => []
  
  fun getCONs ev : datacon list =
       let fun extract (GENtyc{kind=ref (DATAtyc dcl),...},acc) = dcl @ acc
	     | extract (GENtyc{kind=ref (ABStyc tyc),...},acc) = extract (tyc,acc)
	     | extract (_,acc) = acc
	   fun geteb (EBgen {exn,...},acc) = exn::acc
	     | geteb (EBdef {exn,...},acc) = exn::acc
       in
	case ev of
	  TYPEev(MARKdec(DATATYPEdec{datatycs=tycl,...},_,_)) => 
				  fold extract tycl nil
	| TYPEev(MARKdec(ABSTYPEdec{abstycs=tycl,...},_,_)) => 
				  fold extract tycl nil
	| EXCEPTIONev(MARKdec(EXCEPTIONdec ebl,_,_)) => fold geteb ebl nil
	| _ => []
       end
  
  (* finding a named binding of a given sort at a particular (time,sub-event) *)
  fun findAtBind0 getter namer binder (n,t,c) : (int * binding) =
    let fun find(i,v::r) = if Symbol.name (namer v) = n then (i,binder v)
			   else find(i+1,r)
	  | find(_,nil) = raise Unbound
    in find (0,getter(nth(eventsAt t,c)))
    end
  
  val findFIXAtBind = findAtBind0 getFIXs (fn FIXvar{name,...} => name) FIXbind
  val findSIGAtBind = findAtBind0 getSIGs (fn SIGvar{name,...} => name) SIGbind
  val findFSIGAtBind = findAtBind0 getFSIGs (fn FSIGvar{name,...} => name) 
                           FSIGbind
  val findFCTAtBind = findAtBind0 getFCTs (fn FCTvar{name,...} => name) FCTbind
  val findTYCAtBind = findAtBind0 getTYCs TypesUtil.tycName TYCbind
  
  fun findVARCONAtBind ntc =
	 findAtBind0 getVARs 
	     (fn VALvar{name=[nm],...} => nm | OVLDvar{name,...} => name)
		     VARbind ntc
	   handle Unbound =>
	      findAtBind0 getCONs (fn DATACON{name,...} => name) CONbind ntc
  
  fun fctArgAt (t,c) : ((Symbol.symbol option * Structure) option) =
	case nth(eventsAt t,c) of
	  FCTENTev(FCTB{def=FCTfct _,...}) =>
	     (case eventsAt (pred t) of
		((FCTAPPev(MARKstr(APPstr{oper,argexp,...},_,_)))::_) =>
		    SOME(getFctArg(oper,getArgStruct argexp))
	      | _ => debugPanic "Bindings.fctArgAt missing FCTAPPev")
	| _ => NONE

  fun findSTRAtBind (n,t,c) : (int * binding) =
	 findAtBind0 getSTRs (fn STRvar{name=nm,...} => nm) STRbind (n,t,c)
	   handle Unbound => 
		case fctArgAt(t,c) of 
		  SOME(name,s) => 
		   (case name of
		     SOME name =>
		       if Symbol.name name = n then
	  	         (* dummy up a STRvar (??) *)
			 (~1,STRbind(STRvar{name=name,
					    binding=s,
					    access=PATH[0]}))
		       else raise Unbound
		    | NONE => raise Unbound)
		| NONE => raise Unbound

  
  (* Utility for keeping track of local hiding.
     We maintain a pair of integer counters (ec,ic), initially set to (0,0).
     Intuitively, ec is the number of LOCALENDevs encountered not 
     yet matched by a LOCALINev, and ic is the number of LOCALINevs
     that began an invisible region and have not yet been matched by a 
     LOCALev. *)
 fun checkvis(t,c,(ec,ic)) =
    case nth(eventsAt t,c) of
      LOCALev(_) => if ic > 0 then
		      (ec,ic-1)
		    else (ec,ic)
    | LOCINev(_) => if ec > 0 then
		      (ec-1,ic+1)
		    else (ec,ic)
    | LOCENDev(_) => (ec+1,ic)
    | _ => (ec,ic)
  
  (* utility for extracting from run-time arguments *)
  fun nthArgs (evn,n,args) =
    let fun f (_,0,args) = args
	  | f (evt::rest,n,args) = f(rest,n-1,tln (args,argCnt evt))
    in f (eventsFor evn,n,args)
    end
  
  (* extract structure names/bindings from OPEN dec *)
  fun getopennb ev: ((string list * Structure) list) = 
       case ev of
	 OPENev (MARKdec(OPENdec svl,_,_)) =>
		  map (fn STRvar{name,binding,...} => 
			  ([Symbol.name name],binding)) (rev svl)
       | _ => []
  
  (* full scale finder: takes any qualified id; 
			returns binding time as well as binding *)
  fun findBind (findAtBind:(string * time * evindex) -> (int * binding))
	       (findInStruct:(string*Structure) -> binding) :
      (string list * time * evindex) -> (time * evindex * (int * binding)) =
     let fun count t = (length o eventsAt) t
	 fun checkInStruct x = (findInStruct x; true) handle Unbound => false
	 fun find ([n],t,c) =
	       let fun loop (0,_,_) = raise Unbound
		     | loop (t,c,vc as (_,ic)) = 
				   if c < count t then
				     if ic = 0 then
				       (t,c,findAtBind(n,t,c))
					 handle Unbound => 
					  (findInOpen (n,t,c) 
					   handle Unbound =>
					    (findInAnonFctarg (n,t,c) 
					     handle Unbound =>
					      loop (t,c+1,checkvis(t,c,vc))))
				      else loop (t,c+1,checkvis(t,c,vc))
				    else loop(lbtAt t,0,vc)
	       in loop (t,c,(0,0))
	       end
	   | find (n::r,t,c) =
	       let val (t',c',(i',STRbind(STRvar{binding=s,...}))) = 
		      findSTRBind (r,t,c)
	       in if checkInStruct (n,s)
		  then find ([n], enterStruct(t',c',i'),0)
		  else raise Unbound
	       end
	   | find ([],_,_) = raise Unbound
  
	 and findInOpen (n,t,c) =
	       let fun f ((sn,st)::r) = 
		         if checkInStruct (n,st) then 
			   let val (t',c',(i',_)) = findSTRBind(sn,t,c)
			   in find ([n], enterStruct(t',c',i'),0)
			   end
			 else f r
		     | f nil = raise Unbound
	       in f (getopennb (nth(eventsAt t,c)))
	       end
  
	 and findInAnonFctarg (n,t,c) = 
	       case fctArgAt (t,c) of 
		 SOME (name,st) => 
		   if name = NONE andalso checkInStruct(n,st) then
	             find ([n], enterStruct(t,c,~1), 0)
		   else raise Unbound
	       | _ => raise Unbound
     in
       find
     end
  
  
  (* N.B. There is a bug with substructures of constrained structures
      (including all structures in an anonymous functor parameter):
      the top level structure is correctly constrained, but the 
      substructures are not. *)
  and findSTRBind (nl,t,c) = findBind findSTRAtBind findSTRInStruct (nl,t,c)
  
  and enterStruct (t,c,i) : time =
    let
      val _ = if (!debugdebug) then
		 (app say ["entering ",makestring(t:int)," ", 
		       makestring(c:int)," ",makestring(i:int),"\n"])
	      else ()
      val tp = if i < 0 then t-2 (* FCTENTev: jump into parameter *)
	       else (* STRDECev or ABSDECev: extract end of structure time *)
		    let val (evn,args) = evnArgsAt t
			val timearr:int array = 
 	  		       U.cast (hd (nthArgs(evn,c,args)))
		    in timearr sub i
			    handle Nth => 
			      debugPanic "Bindings.enterStruct bad evn c"
		    end
      val ts = case hd (eventsAt tp) of
		 STRVARev (MARKstr(VARstr(STRvar{name,...}),_,_)) =>
		   let val (t',c',(i',_)) = 
		          findSTRBind ([Symbol.name name],tp,0)
		   in enterStruct (t',c',i')
		   end
	       | STRVARev _ => 
		     debugPanic "Bindings.enterStruct bad STRVARev"
	       | STRENDev _ => tp
	       | _ => debugPanic "Bindings.enterStruct bad event"
    in if (!debugdebug) then
	 (app say ["entered at ",makestring(ts:int),"\n"])
       else ();
       ts     
    end
  
  val findVARCONBind = findBind findVARCONAtBind findVARCONInStruct
  val findFCTBind = findBind findFCTAtBind (fn _ => raise Unbound)
  val findSIGBind = findBind findSIGAtBind (fn _ => raise Unbound)
  val findFSIGBind = findBind findFSIGAtBind (fn _ => raise Unbound)
  val findTYCBind = findBind findTYCAtBind findTYCInStruct
  val findFIXBind = findBind findFIXAtBind (fn _ => raise Unbound) 
    
end (* structure *)
