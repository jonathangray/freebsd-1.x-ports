signature SPROF =
  sig
      val instrumDec : Source.inputSource -> Absyn.dec -> Absyn.dec
  end

abstraction SProf : SPROF =
struct

open Access Absyn Variables Modules Types BasicTypes

infix -->
val xsym = Symbol.varSymbol "x"

fun instrumDec source absyn =
 if not(!System.Unsafe.sprofiling) then absyn
 else let 

val namelist : string list ref = ref nil
val namecount = ref 0

val alpha = IBOUND 0

val entervar as VALvar{typ=entertyp,...} = mkVALvar(Symbol.varSymbol "enter")
val _ = entertyp := POLYty{sign=[{weakness=infinity,eq=false}],
			   abs=0,
			   tyfun = TYFUN{arity=1,
					 body=tupleTy[alpha,intTy] --> alpha}}


val enterexp = VARexp(ref entervar, NONE)

fun clean names = names


fun enter((line_a,line_b),names,exp) = 
   let fun dot (a,[z]) = Symbol.name z :: a
	 | dot (a,x::rest) = dot("." :: Symbol.name x :: a, rest)
	 | dot _ = ErrorMsg.impossible "no path in instrexp"
       val (fname,lineno_a,charpos_a) = Source.filepos source line_a
       val (_,lineno_b,charpos_b) = Source.filepos source line_b
       val position = [fname,":",makestring lineno_a,".",
		       makestring charpos_a,"-", makestring lineno_b, ".",
		       makestring charpos_b,":"]
       val name =  implode (position @ dot (["\n"], names))
       val index = !namecount
    in namecount := index + 1;
       namelist := name :: !namelist;
       APPexp(enterexp, ElabUtil.TUPLEexp[exp, INTexp index])
   end		    

fun instrdec (line, names, VALdec vbl) =
    let fun instrvb (vb as VB{pat=VARpat(VALvar{access=INLINE _,...}),...}) =vb
	  | instrvb (vb as VB{pat=CONSTRAINTpat
		    (VARpat (VALvar{access=INLINE _,...}),_),...}) = vb
	  | instrvb (VB{pat as VARpat(VALvar{name=[n],...}), exp,tyvars}) =
	      VB{pat=pat,exp=instrexp(line, n::clean names) exp, tyvars=tyvars}
	  | instrvb (VB{pat as CONSTRAINTpat
			      (VARpat(VALvar{name=[n],...}),_), exp,tyvars}) =
	      VB{pat=pat,exp=instrexp(line, n::clean names) exp, tyvars=tyvars}
	  | instrvb (VB{pat,exp,tyvars}) =
		    VB{pat=pat, exp=instrexp (line,names) exp, tyvars=tyvars}
    in VALdec (map instrvb vbl)
    end
  
  | instrdec (line, names, VALRECdec rvbl) =
    let fun instrrvb (RVB{var as VALvar{name=[n],...}, exp,resultty,tyvars}) =
               RVB{var=var, exp=instrexp (line, n::clean names) exp, 
					resultty=resultty, tyvars=tyvars}
	  | instrrvb _ = ErrorMsg.impossible "VALRECdec in SProf.instrdec"
    in VALRECdec(map instrrvb rvbl)
    end
  | instrdec(line, names, ABSTYPEdec {abstycs,withtycs,body}) = 
	ABSTYPEdec {abstycs=abstycs,withtycs=withtycs, 
		    body=instrdec(line, names, body)}
  | instrdec(line,names, STRdec strbl) = 
             STRdec (map (fn strb => instrstrb(line,names,strb)) strbl)
  | instrdec(line,names, ABSdec strbl) = 
             ABSdec (map (fn strb => instrstrb(line,names,strb)) strbl)
  | instrdec(line,names, FCTdec fctbl) = 
             FCTdec (map (fn fctb => instrfctb(line,names,fctb)) fctbl)
  | instrdec(line,names, LOCALdec(localdec,visibledec)) =
	LOCALdec(instrdec (line,names,localdec), 
		 instrdec (line,names,visibledec))
  | instrdec(line,names, SEQdec decl) = 
        SEQdec (map (fn dec => instrdec(line,names,dec)) decl)
  | instrdec(line,names, MARKdec(dec,a,b)) = 
        MARKdec(instrdec ((a,b),names,dec), a,b)
  | instrdec(line,names, other) = other

and instrstrexp(line, names, STRUCTstr {body,locations,str}) =
      STRUCTstr{body = (map (fn dec => instrdec(line,names,dec)) body),
				    locations=locations,str=str}
  | instrstrexp(line, names, APPstr {oper,argexp,argthin,str}) = 
      APPstr{oper=oper, argexp=instrstrexp(line,names,argexp),
	     argthin=argthin,str=str}
  | instrstrexp(line, names, VARstr x) = VARstr x
  | instrstrexp(line, names, LETstr(d,body)) = 
		LETstr(instrdec(line,names,d), instrstrexp(line,names,body))
  | instrstrexp(line, names,MARKstr(body,a,b)) = 
             MARKstr(instrstrexp((a,b),names,body),a,b)

and instrstrb (line,names, STRB{strvar=strvar as STRvar{name,...},
						  def,thin,constraint}) = 
        STRB{strvar=strvar,def = instrstrexp(line,name::names,def),
				 thin=thin, constraint=constraint}

and instrfctb (line,names, FCTB{fctvar=fctvar as FCTvar{name,...},
				 def=FCTfct{param,def,thin,constraint}}) =
        FCTB{fctvar=fctvar,
             def=FCTfct{param=param,def=instrstrexp(line,name::names,def),
                      thin=thin, constraint=constraint}}
  | instrfctb (line,names,fctb) = fctb

and instrexp(line,names) =
 let fun rule(RULE(p,e)) = RULE(p, iexp e)
     and iexp (RECORDexp(l as _::_)) =
          let fun field(lab,exp) = (lab, iexp exp)
           in enter(line,Symbol.varSymbol(makestring(length l))::names,
		       RECORDexp(map field l))
          end
       | iexp (VECTORexp l) = VECTORexp(map iexp l)
       | iexp (SEQexp l) = SEQexp(map iexp l)
       | iexp (APPexp(f,a)) = APPexp(iexp f, iexp a)
       | iexp (CONSTRAINTexp(e,t)) = CONSTRAINTexp(iexp e, t)
       | iexp (HANDLEexp (e, HANDLER(FNexp(l,t)))) = 
	          HANDLEexp(iexp e, HANDLER(FNexp(map rule l, t)))
       | iexp (HANDLEexp (e, HANDLER h)) = HANDLEexp(iexp e, HANDLER(iexp h))
       | iexp (RAISEexp(e,t)) = RAISEexp(iexp e, t)
       | iexp (LETexp(d,e)) = LETexp(instrdec(line,names,d), iexp e)
       | iexp (CASEexp(e,l)) = CASEexp(iexp e, map rule l)
       | iexp (FNexp(l,t)) = enter(line,names,(FNexp(map rule l, t)))
       | iexp (MARKexp(e,a,b)) = MARKexp(instrexp((a,b),names) e, a,b)
       | iexp (e as CONexp(DATACON{rep=UNTAGGED,...},_)) = etaexpand e
       | iexp (e as CONexp(DATACON{rep=TAGGED _,...},_)) = etaexpand e
       | iexp (e as CONexp(DATACON{rep=TAGGEDREC _,...},_)) = etaexpand e
       | iexp (e as CONexp(DATACON{rep=REF,...},_)) = etaexpand e
       | iexp (e as CONexp(DATACON{rep=VARIABLE _,...},_)) = etaexpand e
       | iexp e = e 

     and etaexpand(e as CONexp(_,t)) = 
	 let val v = VALvar{access=PATH[mkLvar()], name=[xsym], 
	                    typ=ref Types.UNDEFty}
	  in FNexp([RULE(VARpat v, 
			 enter(line,names,APPexp(e,VARexp(ref v, NONE))))],
		   Types.UNDEFty)
	 end
  in iexp
 end


val derefop = VALvar{name = [Symbol.varSymbol "!"],
		     access = INLINE P.DEREF,
		     typ = ref(POLYty{sign=[{weakness=infinity,eq=false}],
				      abs=0,
				      tyfun = TYFUN{arity=1,
						    body=
						      CONty(refTycon,[alpha]) 
						      --> alpha}})}

val registerTy =  
    POLYty{sign=[{weakness=infinity,eq=false}],
	   abs=0,
	   tyfun = TYFUN{arity=1,
			 body= CONty(refTycon,[stringTy -->
					       (tupleTy[alpha,intTy] 
						--> alpha)])}}

val registerVar = VALvar{name = [Symbol.varSymbol "profile_sregister"],
			 access = PATH(!CoreInfo.sregisterPath),
			 typ = ref registerTy}
val absyn' =instrdec((0,0),nil,absyn) 

in 
   LOCALdec(VALdec[VB{pat=VARpat entervar,
		      exp=APPexp(APPexp(VARexp(ref derefop,NONE),
					VARexp(ref(registerVar),NONE)),
				 STRINGexp(implode(rev(!namelist)))),
		      tyvars=ref nil}],
	     absyn')

end

end

