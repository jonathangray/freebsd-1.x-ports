signature LINKAGE = 
sig
  val getvars: Absyn.dec -> Access.lvar list
end

structure Linkage : LINKAGE =
struct

 (* functions for retrieving new bound lvars from declaration abstract syntax *)

  open Array List Access Variables Types Modules Absyn ErrorMsg
  infix 9 sub

  fun smash f l = fold (fn (a,c) => f a @ c) l []

  fun patvars (VARpat(VALvar{access=PATH[v],...})) = [v]
    | patvars (VARpat(VALvar{access=INLINE _,...})) = []
    | patvars (VARpat _ ) = impossible "non-PATH in translate.patvars"
    | patvars (RECORDpat{fields,...}) = smash (fn (_,p) => patvars p) fields
    | patvars (VECTORpat (pats, _)) = smash patvars pats
    | patvars (APPpat(_,_,p)) = patvars p
    | patvars (CONSTRAINTpat(p,_)) = patvars p
    | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q
    | patvars _ = []

  fun getvars (VALdec vbl) = smash (fn VB{pat,...} => patvars pat) vbl
    | getvars (a as VALRECdec rvbl) =
	smash (fn RVB{var=VALvar{access=PATH[var],...},exp,...} => [var]
	        | _ => impossible "#738 in translate")
	      rvbl
    | getvars (LOCALdec (localdec,visibledec)) = 
	(* it's necessary to "getvars localdec" in case the visibledec
		contains an "open".  Yuck. *)
		(getvars localdec @ getvars visibledec)
    | getvars (EXCEPTIONdec ebl) =
	map (fn EBgen{exn=DATACON{rep=VARIABLE(PATH[v]),...},...} => v
	      | EBdef{exn=DATACON{rep=VARIABLE(PATH[v]),...},...} => v
	      | EBgen{exn=DATACON{rep=VARIABLEc(PATH[v]),...},...} => v
	      | EBdef{exn=DATACON{rep=VARIABLEc(PATH[v]),...},...} => v
	      | _ => impossible "in getvars EXCEPTIONdec")
	    ebl
    | getvars (SEQdec decl) = smash getvars decl
    | getvars (DATATYPEdec _) = []
    | getvars (ABSTYPEdec{body,...}) = getvars body
    | getvars (TYPEdec _) = []
    | getvars (STRdec sbl) =
	map (fn STRB{strvar=STRvar{access=PATH[v],...},...} => v
	      | _ => impossible "getvars(STRdec)/fn"
	    ) sbl
    | getvars (ABSdec sbl) =
	map (fn STRB{strvar=STRvar{access=PATH[v],...},...} => v
	      | _ => impossible "getvars(ABSdec)/fn"
	    ) sbl
    | getvars (FCTdec fbl) =
	map (fn FCTB{fctvar=FCTvar{name,access=PATH[v],...},...} => v
	      | _ => impossible "getvars(FCTdec)/fn"
	    ) fbl
    | getvars (FIXdec _) = []
    | getvars (OVLDdec _) = []
    | getvars (OPENdec _) = []
    | getvars (SIGdec _) = []
    | getvars (FSIGdec _) = []
    | getvars (MARKdec (dec,_,_)) = getvars dec

end (* structure Linkage *)
