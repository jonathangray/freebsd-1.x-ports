(* prof.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)

signature PROF =
  sig
    val instrumDec : Absyn.dec -> Absyn.dec
    val instrumStrb : Absyn.strb -> Absyn.strb
    val instrumFctb : Absyn.fctb -> Absyn.fctb
  end

abstraction Prof : PROF =
struct

open Access Absyn ElabUtil Lambda Variables Modules Types BasicTypes ErrorMsg

infix -->
val varSymbol = Symbol.varSymbol

(* Profiling globals *)
val profiling = System.Unsafe.profiling
val profileList = ref([]: (Access.lvar * string) list)
		
val anonSym = varSymbol "anon"

val intreftype = CONty(refTycon,[intTy])

val alpha = IBOUND 0

fun register() = VALvar{name = [varSymbol "profile_register"],
		      access = PATH(!CoreInfo.registerPath),
		      typ = ref alpha}
val updateOp = VALvar{name = [varSymbol "unboxedupdate"],
	           access = INLINE P.UNBOXEDUPDATE,
		   typ = ref (tupleTy[intreftype,intTy,intTy] --> unitTy)}
fun updateExp (a,k,b) = APPexp(VARexp(ref updateOp,NONE), TUPLEexp[a, k, b])

val subop =  VALvar{name = [varSymbol "subscript"],
		     access = INLINE P.SUBSCRIPT,
		     typ = ref(CONty(Tuples.mkTUPLEtyc 2,
				     [CONty(arrayTycon,[alpha]),
				      intTy])
			       --> alpha)}

val derefop = VALvar{name = [varSymbol "!"],
		     access = INLINE P.DEREF,
		     typ = ref(CONty(refTycon,[alpha]) --> alpha)}

val addop = VALvar{name = [varSymbol "iadd"],
		   access = INLINE P.IADD,
		   typ = ref(tupleTy[intTy,intTy] --> intTy)}

fun tmpvar str = mkVALvar (varSymbol str)

fun clean (path as name::names) = if Symbol.eq(name,anonSym) then names else path
  | clean x = x

fun instrumDec' absyn =
 let val countarrayvar = tmpvar "countarray"
     val countarray = VARexp(ref countarrayvar,NONE)
     val basevar = tmpvar "base"
     val baseexp = VARexp(ref basevar,NONE)
     val currentvar = tmpvar "current"
     val currentexp = VARexp(ref currentvar,NONE)
     
     val entries = ref (nil: string list)
     val entrycount = ref 0
     fun makeEntry(name) = let val i = !entrycount
	                    in entries := "\n" :: name :: !entries;
			       entrycount := i+1;
			       i
			   end

     fun BUMPCCexp ccvara = 
	 let val lvar = tmpvar("indexvar")
	  in updateExp (countarray,
			INTexp ccvara,
			APPexp(
			  VARexp(ref addop,NONE), 
			    TUPLEexp[
			      APPexp(VARexp(ref subop,NONE),
				     TUPLEexp[countarray, INTexp ccvara]),
				     INTexp 1]))
	 end
     fun SETCURRENTexp ccvara =
	 let val lvar = tmpvar("indexvar")
	  in LETexp(VALdec[VB{pat=VARpat(lvar),
			    exp=APPexp(VARexp(ref addop,NONE),
				       TUPLEexp[INTexp ccvara,
						baseexp]),
			    tyvars=ref nil}],
		  updateExp (currentexp,
			     INTexp 0,
			     VARexp(ref lvar,NONE)))
	 end

   fun instrdec(sp as (names,ccvara), VALdec vbl) = 
    let fun instrvb (vb as VB{pat=VARpat(VALvar{access=INLINE _,...}),...}) = vb
	  | instrvb (vb as VB{pat=CONSTRAINTpat
		    (VARpat (VALvar{access=INLINE _,...}),_),...}) = vb
	  | instrvb (VB{pat as VARpat(VALvar{access=PATH[v],name=[n],...}),
			exp,tyvars}) =
	      VB{pat=pat,exp=instrexp (n::clean names,ccvara) false exp,
		 tyvars=tyvars}
	  | instrvb (VB{pat as CONSTRAINTpat
			      (VARpat(VALvar{access=PATH[v],name=[n],...}),_),
			exp,tyvars}) =
	      VB{pat=pat,exp=instrexp (n::clean names,ccvara) false exp,
		 tyvars=tyvars}
	  | instrvb (VB{pat,exp,tyvars}) =
		    VB{pat=pat, exp=instrexp sp false exp, tyvars=tyvars}
    in VALdec (map instrvb vbl)
    end
  | instrdec(sp as (names,ccvara), VALRECdec rvbl) = 
    let fun instrrvb (RVB{var=var as VALvar{access=PATH[v], name=[n],...},
							exp,resultty,tyvars}) =
               RVB{var=var, exp=instrexp (n::clean names, ccvara) false exp, 
					resultty=resultty, tyvars=tyvars}
	  | instrrvb _ = impossible "VALRECdec in instrdec"
    in VALRECdec(map instrrvb rvbl)
    end
  | instrdec(sp, ABSTYPEdec {abstycs,withtycs,body}) = 
	ABSTYPEdec {abstycs=abstycs,withtycs=withtycs, body=instrdec(sp,body)}
  | instrdec(sp, STRdec strbl) = STRdec (map (fn strb => instrstrb(sp,strb)) strbl)
  | instrdec(sp, ABSdec strbl) = ABSdec (map (fn strb => instrstrb(sp,strb)) strbl)
  | instrdec(sp, FCTdec fctbl) = FCTdec (map (fn fctb => instrfctb(sp,fctb)) fctbl)
  | instrdec(sp, LOCALdec(localdec,visibledec)) =
	LOCALdec(instrdec (sp,localdec), instrdec (sp,visibledec))
  | instrdec(sp, SEQdec decl) = SEQdec (map (fn dec => instrdec(sp,dec)) decl)
  | instrdec(sp, MARKdec(dec,a,b)) = MARKdec(instrdec (sp,dec), a,b)
  | instrdec(sp, other) = other

and instrstrexp(names, STRUCTstr {body,locations,str}) =
      STRUCTstr{body = (map (fn dec => instrdec((names,0),dec)) body),
				    locations=locations,str=str}
  | instrstrexp(names, APPstr {oper,argexp,argthin,str}) = 
      APPstr{oper=oper, argexp=instrstrexp(names,argexp),argthin=argthin,str=str}
  | instrstrexp(names, VARstr x) = VARstr x
  | instrstrexp(names, LETstr(d,body)) = 
		LETstr(instrdec((names,0),d), instrstrexp(names,body))
  | instrstrexp(names,MARKstr(body,a,b)) = MARKstr(instrstrexp(names,body),a,b)

and instrstrb ((names,ccvara), STRB{strvar=strvar as STRvar{name,...},
						  def,thin,constraint}) = 
        STRB{strvar=strvar,def = instrstrexp(name::names,def),
				 thin=thin, constraint=constraint}

and instrfctb ((names,ccvara), FCTB{fctvar=fctvar as FCTvar{name,...},
				    def=FCTfct{param,def,thin,constraint}}) =
        FCTB{fctvar=fctvar,
             def=FCTfct{param=param,def=instrstrexp(name::names,def),
                      thin=thin, constraint=constraint}}
  | instrfctb ((names,ccvara),fctb) = fctb

and instrexp(sp as (names,ccvara)) =
 let fun istail tail =
     let fun iinstr exp = istail false exp
	 fun oinstr exp = istail true exp
	 fun instrrules tr = map (fn (RULE(p,e)) => RULE(p, tr e))
	 val rec instr:(exp->exp) =
	     fn RECORDexp l => RECORDexp(map (fn (lab,exp) => (lab,iinstr exp)) l)
	      | VECTORexp l => VECTORexp(map iinstr l)
	      | SEQexp l =>
		let fun seq [e] = [instr e]
		      | seq (e::r) = (iinstr e)::(seq r)
		      | seq nil = nil
		in SEQexp (seq l)
		end
	      | APPexp (f,a) =>
	        let fun safe(VARexp(ref(VALvar{access=INLINE P.CALLCC,
                                               ...}),_)) = false
		      | safe(VARexp(ref(VALvar{access=INLINE _,
                                               ...}),_)) = true
		      | safe _ = false
 		 in if tail orelse (safe f)
		    then APPexp (iinstr f, iinstr a)
		    else let val lvar = tmpvar("appvar")
			  in LETexp (VALdec[VB{pat=VARpat(lvar),
					       exp=APPexp(iinstr f, iinstr a),
					       tyvars=ref nil}],
			     SEQexp([SETCURRENTexp(ccvara), 
                                     VARexp(ref lvar,NONE)]))
			 end
		end
	      | CONSTRAINTexp(e,t) => CONSTRAINTexp(instr e, t)
	      | HANDLEexp (e, HANDLER(FNexp(l,t)))=> 
		let fun rule(RULE(p,e)) = 
				RULE(p,SEQexp[SETCURRENTexp ccvara, instr e])
		in HANDLEexp (instr e, HANDLER(FNexp(map rule l,t)))
		end
	      | RAISEexp(e,t) => RAISEexp(oinstr e,t)
	      | LETexp (d,e) => LETexp (instrdec(sp,d), instr e)
	      | CASEexp (e,l) => CASEexp(iinstr e, instrrules instr l)
	      | FNexp(l,t) =>
		let fun dot (a,[z]) = Symbol.name z :: a
		      | dot (a,x::rest) = dot("." :: Symbol.name x :: a, rest)
		      | dot _ = impossible "no path in instrexp"
		    val name =  implode (dot ([], names))
		    val ccvara' = makeEntry(name)
		    val lvar = tmpvar "fnvar";
		in FNexp ([RULE(VARpat(lvar), 
		                SEQexp ([BUMPCCexp(ccvara'),
					 SETCURRENTexp(ccvara'),
					 CASEexp(VARexp(ref lvar,NONE),
					 instrrules (instrexp (anonSym::names,
					                 ccvara') true) l)])),
		           RULE(WILDpat,RECORDexp nil)],t)
		end
	      | MARKexp(e,a,b) => MARKexp(instr e, a, b)
	      | e => e 
     in instr
     end
 in istail
 end

   val absyn' = instrdec(([],0),absyn)

in LOCALdec(VALdec[VB{pat=TUPLEpat[VARpat basevar,
				   VARpat countarrayvar,
				   VARpat currentvar],
		      exp=APPexp(APPexp(VARexp(ref derefop,NONE),
					VARexp(ref(register()),NONE)),
				 STRINGexp(implode(rev(!entries)))),
		      tyvars=ref nil}],
	    absyn')
end

fun instrumDec absyn = 
    if !profiling then instrumDec' absyn else absyn

fun instrumStrb absyn = absyn

fun instrumFctb absyn = absyn

end (* structure Instrum *)
