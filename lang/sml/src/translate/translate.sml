(* Copyright 1989 by AT&T Bell Laboratories *)

signature TRANSLATE = sig
   val transDec : Modules.env -> (Source.region -> ErrorMsg.complainer) ->
                  (Source.region -> string) -> Absyn.dec -> Lambda.lexp ->
		  Lambda.lexp
end

structure Translate : TRANSLATE =
struct

open Access Absyn Lambda Types Modules Variables BasicTypes ElabUtil
     Nonrec ErrorMsg Unboxed Transtypes TransBinding PrettyPrint

(**************************************************************************
 * CONSTANTS AND UTILITY FUNCTIONS.                                       *
 **************************************************************************)
val unitLexp = RECORD[]
fun ident x = x
val matchsym = Symbol.varSymbol("Match")
val bindsym = Symbol.varSymbol("Bind")
val printDepth = System.Print.printDepth

val elemgtr = (fn ((LABEL{number=x,...},_),(LABEL{number=y,...},_))=> x>y);
val sorted = Sort.sorted elemgtr
val sortrec = Sort.sort elemgtr

fun unwrapOp1(lt,NONE) = ident
  | unwrapOp1(lt,SOME tt) = unwrapOp(lt,(transTyLty tt))

fun unwrapOp2(tt,NONE) = ident
  | unwrapOp2(tt,SOME tt') = unwrapOp(transTyLty(tt),transTyLty(tt'))

fun mergety(t1,NONE) = t1
  | mergety(t1,SOME t2) = t2

fun transDcon(DATACON{name,rep,typ,...}) = 
                 (name,rep,transTyLty typ)
val trueDcon' = transDcon trueDcon 
val falseDcon' = transDcon falseDcon

fun composeNOT (eq,t) =  
    let val v = mkLvar()
        val (argt,_) = checkArrowTy(t)
     in FN(v,argt,SWITCH(APP(eq, VAR v),boolsign,
                    [(DATAcon falseDcon', CON(trueDcon',unitLexp)),
		     (DATAcon trueDcon', CON(falseDcon',unitLexp))],NONE))
    end

fun getEqualElem (CONty(_,[CONty(_,[t,_]),_])) = t
  | getEqualElem _ = IBOUND 0

(* generate the lambda code for ":=" using the given update primop.
 * ":=" ==> "fn x => update(#1 x, 0, #2 x)"
 *)
val updateTy = ARROWty(RECORDty[BOXEDty,INTty,RBOXEDty],INTty)
val assignTy = ARROWty(RECORDty[BOXEDty,RBOXEDty],INTty)
fun updateOp(updPrimOp) = 
   let val x = mkLvar() 
       val varX = VAR x
       val argty = RECORDty([BOXEDty,BOXEDty])
    in FN(x,argty,APP(PRIM (updPrimOp,updateTy),
	              RECORD[SELECT(0,varX),INT 0,SELECT(1, varX)]))
   end

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = impossible "translate.translatepath nil"


(**************************************************************************
 *    transDec : Modules.env -> (Source.region -> ErrorMsg.complainer)    *
 *                           -> (Source.region -> string)                 *
 *                           -> Absyn.dec -> Lambda.lexp -> Lambda.lexp   *
 **************************************************************************)
fun transDec env err errorMatch rootdec =
let 
 val polyequal = translatepath(!CoreInfo.polyequalPath)

(* The function fill : Source.region -> Absyn.pat -> Absyn.pat expands the
 * flexible record pattern. 
 *)
 fun fill loc =
  let fun f (APPpat(_,_,p)) = f p
	| f (CONSTRAINTpat (p,_)) = f p
	| f (LAYEREDpat (p,q)) = (f p; f q)
	| f (RECORDpat {pats = ref (_::_),...}) = ()
	| f (RECORDpat {fields,flex=false,pats,...}) =
	       pats := map (fn (_,p) => (f p; p)) fields
	| f (pat as RECORDpat {fields,flex=true,typ,pats}) =
	      (app (fn (_,p) => f p) fields;
	       let exception DontBother
		   fun find (t as CONty(RECORDtyc labels, _)) = 
				      (typ := t; labels)
		     | find _ = (err loc COMPLAIN "unresolved flexible record"
				   (fn ppstrm =>
				    (add_newline ppstrm;
				     add_string ppstrm "pattern: ";
				     PPAbsyn.ppPat env ppstrm (pat,!printDepth)));
				 raise DontBother)
		   fun merge (a as ((id,p)::r), lab::s) =
			 if Symbol.eq(id,lab) then p :: merge(r,s) 
			 else WILDpat :: merge(a,s)
		     | merge (nil, lab::s) = WILDpat :: merge(nil,s)
		     | merge (nil,nil) = nil
		     | merge _ = impossible "merge in translate"
		in pats := (merge(fields,
				  find(TypesUtil.headReduceType (!typ)))
				handle DontBother => [WILDpat])
	       end)
        | f (VECTORpat(pats,_)) = app f pats
        | f (ORpat(p1, p2)) = (f p1; f p2)
	| f _ = ()
   in f
  end
	  
(**************************************************************************
 * STRUCTURE AND FUCTOR THINING FUNCTIONS.                                *
 **************************************************************************)
 fun thinStr(e,NONE) = e
   | thinStr(e,SOME(v,locs)) = 
       APP(FN(v,BOGUSty,RECORD(map transLoc locs)), e)

 and thinFct(e,_,NONE,NONE) = e
   | thinFct(e,ty,SOME (v1,locs1),SOME (v2,locs2)) =
        FN(v1,ty,APP(FN(v2,BOGUSty,RECORD(map transLoc locs2)),
                     APP(e,RECORD(map transLoc locs1))))
   | thinFct(e,ty,NONE,SOME(v,locs)) =
        let val w = mkLvar ()
         in FN(w,ty,APP(FN(v,BOGUSty,RECORD(map transLoc locs)), 
                        APP(e,VAR w)))
	end
   | thinFct (e,ty,SOME(v1,locs1),NONE) =
        FN(v1,ty,APP(e,RECORD(map transLoc locs1)))

(**************************************************************************
 *               transLoc : Modules.trans -> Lambda.lexp                  *
 **************************************************************************)
 and transLoc(VALtrans(acc,t1,t2)) =
     (case acc 
       of (PATH p) => 
              let val header = unwrapOp2(t1,t2)
               in header(translatepath p)
              end
        | (INLINE P.POLYEQL) => 
              let val t = mergety(t1,t2)
               in Equal.equal env (getEqualElem(t))
              end
        | (INLINE P.POLYNEQ) => 
              let val t = mergety(t1,t2)
                  val eq = Equal.equal env (getEqualElem(t))
               in composeNOT(eq,transTyLty(t))
              end
        | (INLINE P.INLSUBSCRIPTV) => 
             let val lt = transTyLty(mergety(t1,t2))
              in InlineOps.inlsubscriptv(lt)
             end
        | (INLINE P.INLSUBSCRIPT) => 
             let val lt = transTyLty(mergety(t1,t2))
              in InlineOps.inlsubscript(lt)
             end
        | (INLINE P.INLUPDATE) => 
             let val t = mergety(t1,t2)
                 val lt = transTyLty(t) 
                 val header = unwrapOp(updateTy,lt)
                 val oper = header(PRIM(unboxedUpdate(t),updateTy))
              in (InlineOps.inlupdate(lt,oper))
             end
        | (INLINE P.INLBYTEOF) => InlineOps.inlbyteof()
        | (INLINE P.INLSTORE) => InlineOps.inlstore()
        | (INLINE P.INLORDOF) => InlineOps.inlordof()
        | (INLINE P.INLFSUBSCRIPTd) => InlineOps.inlsubscriptf()
        | (INLINE P.INLFUPDATEd) => InlineOps.inlupdatef()
        | (INLINE P.ASSIGN) => 
             let val t = mergety(t1,t2)
                 val header = unwrapOp(assignTy,transTyLty(t))
              in header(updateOp(unboxedAssign(t)))
             end
        | (INLINE i) =>          (* may need rearrangement here *)
             let val lt = transTyLty(t1)
                 val header = unwrapOp1(lt,t2)
              in header(PRIM(i,lt))
             end
        | _ =>
	   (ErrorMsg.impossibleWithBody "transLoc.1"
	     (fn ppstrm =>
	      (add_string ppstrm "transLoc: ";
	       PPBasics.ppAccess ppstrm acc;
	       add_newline ppstrm))))


   | transLoc(THINtrans(PATH p,v,locs)) =
          thinStr(translatepath p, SOME(v,locs))
   | transLoc(FCTtrans(PATH p,s,thin1,thin2)) =
          let val lt = transSigLty(s) 
           in thinFct(translatepath p,lt,thin1,thin2)
          end
   | transLoc(CONtrans(d as DATACON{const=true,typ=t1,...},t2)) = 
          let val header = unwrapOp2(t1,t2)
           in header(CON'(transDcon d,unitLexp))
          end
   | transLoc(CONtrans(d as DATACON{const=false,typ=t1,...},t2)) =
          let val v = mkLvar()
              val lt1 = transTyLty(t1)
              val (argt,_) = checkArrowTy(lt1)
              val header = unwrapOp1(lt1,t2)
           in header(FN(v,argt,CON'(transDcon d, VAR v)))
          end
   | transLoc(THINtrans(a,_,_)) = 
       ErrorMsg.impossibleWithBody "transLoc.2"
	(fn ppstrm =>
	  (add_string ppstrm "transLoc: ";
	   PPBasics.ppAccess ppstrm a; add_newline ppstrm))
   | transLoc(FCTtrans(a,_,_,_)) = 
       ErrorMsg.impossibleWithBody "transLoc.3"
	(fn ppstrm =>
	  (add_string ppstrm "transLoc: ";
	   PPBasics.ppAccess ppstrm a; add_newline ppstrm))

 
(**************************************************************************
 *       transStr : Source.region -> Absyn.strexp -> Lambda.lexp          *
 **************************************************************************)
 and transStr loc sdec =
   case sdec
    of VARstr(STRvar{access=PATH(path),...}) => translatepath path
     | STRUCTstr{body,locations,...} =>
	 makedec loc (SEQdec body) (RECORD(map transLoc locations))
     | APPstr{oper=FCTvar{access=PATH(path),...},argexp,argthin,...} =>
         APP(translatepath path, thinStr(transStr loc argexp, argthin))
     | LETstr(d,body) => makedec loc d (transStr loc body)
     | MARKstr(body,a,b) => transStr (a,b) body
     | _ => impossible "Translate.transStr"


(**************************************************************************
 *   makedec : Source.region -> Absyn.dec -> Lambda.lexp -> Lambda.lexp   *
 **************************************************************************) 
 and makedec loc dec =
   case dec
   of VALdec vbl =>
      fold (fn (VB{pat=VARpat(VALvar{access=INLINE(_),...}),...},b) => b
	     | (VB{pat=CONSTRAINTpat(VARpat(VALvar{access=INLINE _,...}),_),
		   exp=_,...},b) => b
	     | (VB{pat=VARpat(VALvar{access=PATH[v],...}),
                   exp,...},b) => APP(FN(v,BOGUSty,b),translate loc exp)
	     | (VB{pat,exp,...},b) => 
                let val ee = translate loc exp
                    val en = transDcon(!CoreInfo.exnBind)
	            val _ = fill loc pat 
		 in APP(MC.bindCompile env
			 ([(pat,b),
			   (WILDpat,
			    case CoreInfo.errorMatchPath
			      of ref ERRORvar =>
				   RAISE(CON'(en,unitLexp),BOXEDty)
			       | varError =>
				   translate loc
				     (SEQexp[APPexp
					      (VARexp(ref Prim.assignVar,NONE),
					       TUPLEexp[VARexp (varError,NONE),
						 STRINGexp (errorMatch loc)]),
					     RAISEexp
					       (CONexp(!CoreInfo.exnBind,NONE),
						UNDEFty)]))],
			  BOGUSty,
			  err loc),
			ee)
                end)
	  vbl

    | a as VALRECdec rvbl =>
       (makedec loc (nonrec a)
	handle Isrec =>
	(fn e => FIX(fold
	 (fn (RVB{var=VALvar{access=PATH[v],typ=ref ty,...},exp,...}, 
                                             (vlist,tlist,elist,lexp)) =>
            (let val ee = translate loc exp
                 val vt = transTyLty ty
              in (v::vlist, vt::tlist, ee::elist, lexp)
             end)
	   | _ => impossible "#73 in translate")
	 rvbl (nil,nil,nil,e))))

    | LOCALdec(localdec,visibledec) =>
          makedec loc (SEQdec[localdec,visibledec])

    | EXCEPTIONdec ebl =>
      fold(fn (EBgen{exn=DATACON{rep=VARIABLE(PATH[v]),...},ident,...},lexp)=>
        	 APP(FN(v,BOGUSty,lexp),
		     CON'(transDcon refDcon,translate loc ident))
	    | (EBgen{exn=DATACON{rep=VARIABLEc(PATH[v]),...},ident,...},lexp)=>
		 APP(FN(v,BOGUSty,lexp),
		     RECORD([CON'(transDcon refDcon, translate loc ident),
                            unitLexp]))
	    | (EBdef{exn=DATACON{rep=VARIABLE(PATH[v]),...},
		     edef=DATACON{rep=VARIABLE(PATH p),...}}, lexp) => 
                 APP(FN(v,BOGUSty,lexp),translatepath p)
	    | (EBdef{exn=DATACON{rep=VARIABLEc(PATH[v]),...},
	             edef=DATACON{rep=VARIABLEc(PATH p),...}}, lexp) => 
                 APP(FN(v,BOGUSty,lexp),translatepath p)
	    | _ => impossible "in makedec EXCEPTIONdec")
        ebl
 
     | SEQdec decl =>
        let fun f(a::r) = (makedec loc a) o (f r) 
              | f nil = ident
         in f decl 
        end
     | DATATYPEdec _ => ident
     | ABSTYPEdec{body,...} => makedec loc body
     | TYPEdec _ => ident
     | STRdec sbl =>
         fold(fn (STRB{strvar=STRvar{access=PATH[v],...},def,thin,...},lexp) =>
	           APP(FN(v,BOGUSty,lexp),thinStr(transStr loc def, thin))
	       | _ => impossible "makedec(STRdec) in translate")
	   sbl
     | ABSdec sbl => makedec loc (STRdec sbl)
     | FCTdec fbl =>
	 let fun transFct t (FCTfct{def,thin,param=STRvar{access=PATH[p],...},
				    ...}) =
			FN(p,t,thinStr(transStr loc def, thin))
		| transFct t (VARfct{thinIn, thinOut,
                                     def=FCTvar{access=PATH p,...},...}) =
		     thinFct(translatepath p,t,thinIn,thinOut)
		| transFct t (LETfct(dec,body)) =
		     makedec loc dec (transFct t body)
	 in  fold
	       (fn (FCTB{fctvar = FCTvar{access=PATH[v],binding,...},
			 def,...}, lexp) =>
		      let val (t,_) = checkArrowTy(transFctLty(binding))
		      in APP(FN(v,BOGUSty,lexp), transFct t def) end
		 | _ => impossible "makedec(FCTdec) in translate")
	       fbl
	 end
     | SIGdec _ => ident
     | FSIGdec _ => ident 
     | OPENdec _ => ident
     | FIXdec _ => ident
     | OVLDdec _ => ident
     | MARKdec(dec,a,b) => makedec (a,b) dec 

 and transrules loc rules = 
     let fun f (RULE(p,e)) = ((fill loc p; p), translate loc e) 
      in map f rules 
     end


(**************************************************************************
 *        translate : Source.region -> Absyn.exp -> Lambda.lexp           *
 **************************************************************************) 
 and translate loc exp =
   case exp 
    of INTexp i => INT i
     | REALexp r => REAL r
     | STRINGexp s => STRING s
     | RECORDexp nil => INT 0
     | RECORDexp l =>
	 if sorted l
	 then let val lexpl = map (fn (_,e) => translate loc e) l
               in RECORD lexpl
              end
	 else let val vars = map (fn (l,e) => (l,(translate loc e,mkLvar()))) l
		  fun bind ((_,(e,v)),x) = APP(FN(v,BOGUSty,x),e)
                  val bexp = map (fn (_,(_,v)) => VAR v) (sortrec vars)
	       in fold bind vars (RECORD bexp)
	      end
     | VECTORexp nil => translatepath(!CoreInfo.vector0Path)
     | VECTORexp l => 
	 let val vars = map (fn e => (e,mkLvar())) l
             fun bind ((e,v),x) = APP(FN(v,BOGUSty,x), translate loc e)
          in fold bind vars (VECTOR(map (fn(_,v)=>VAR v) vars))
         end 
     | SEQexp [e] => translate loc e
     | SEQexp (e::r) => 
         let val le = translate loc e
             val re = translate loc (SEQexp r)
          in APP(FN(mkLvar(),BOGUSty,re),le)
         end
     | APPexp(e1 as CONexp(dcon as DATACON{rep=CONSTANT _,...},_), e2) =>
         APP(translate loc e1, translate loc e2)
     | APPexp(CONexp(dcon as DATACON{name,rep,typ,...},NONE), e) => 
         let val le = translate loc e
             val lt = transTyLty(typ)  
          in CON'((name,rep,lt),le)
         end        
     | MARKexp(e,a,b) => translate (a,b) e
     | CONexp(dcon as DATACON{const=false,typ=ty,name=name,rep=rep,...},tt) =>
         let val v = mkLvar()
             val lt = transTyLty(ty)
             val (t1,_) = checkArrowTy(lt)
             val header = unwrapOp1(lt,tt)
          in header(FN(v,t1,CON'((name,rep,lt),VAR v)))
         end
     | CONexp (dcon as DATACON{const=true,typ=ty,name=name,rep=rep,...},tt) => 
         let val lt = transTyLty(ty) 
             val header = unwrapOp1(lt,tt)
          in header(CON'((name,rep,lt), unitLexp))
         end
     | VARexp (ref(VALvar{access=PATH(path),...}),NONE) => translatepath path
     | VARexp (ref(VALvar{access=PATH(path),typ=ref t1,...}),SOME t2) => 
         let val lt1 = transTyLty(t1)
             val lt2 = transTyLty(t2)
             val header = unwrapOp(lt1,lt2)
          in header(translatepath path)
         end
     | VARexp (ref(VALvar{access=INLINE P.POLYEQL,typ=ref ty,...}),_) => 
	 Equal.equal env (getEqualElem(ty))
     | VARexp (ref(VALvar{access=INLINE P.POLYNEQ,typ=ref ty,...}),_) => 
         let val t = transTyLty(ty)
             val eq = Equal.equal env (getEqualElem(ty))
	  in composeNOT(eq,t)
         end
     | VARexp (ref(VALvar{access=INLINE P.ASSIGN, typ=ref ty,...}),_) =>
         let val t = transTyLty(ty)
             val header = unwrapOp(assignTy,t)
          in header(updateOp(unboxedAssign(ty)))
         end
     | VARexp (ref(VALvar{access=INLINE P.UPDATE, typ=ref ty,...}),_) => 
         let val t = transTyLty(ty)
             val header = unwrapOp(updateTy,t)
          in header(PRIM(unboxedUpdate(ty),updateTy))
         end
     | VARexp (ref(VALvar{access=INLINE P.INLSUBSCRIPT,typ=ref ty,...}),_) =>
         let val t = transTyLty(ty)
          in InlineOps.inlsubscript(t)
         end
     | VARexp (ref(VALvar{access=INLINE P.INLSUBSCRIPTV,typ=ref ty,...}),_) =>
         let val t = transTyLty(ty)
          in InlineOps.inlsubscriptv(t)
         end
     | VARexp (ref(VALvar{access=INLINE P.INLUPDATE, typ=ref ty, ...}),_) =>
         let val t = transTyLty(ty)
             val header = unwrapOp(updateTy,t)              
             val oper = header(PRIM(unboxedUpdate(ty),updateTy))
          in InlineOps.inlupdate(t,oper)
         end
     | VARexp (ref(VALvar{access=INLINE P.INLBYTEOF,...}),_) =>
         InlineOps.inlbyteof() 
     | VARexp (ref(VALvar{access=INLINE P.INLSTORE,...}),_) =>
         InlineOps.inlstore() 
     | VARexp (ref(VALvar{access=INLINE P.INLORDOF,...}),_) =>
         InlineOps.inlordof() 
     | VARexp (ref(VALvar{access=INLINE P.INLFSUBSCRIPTd,...}),_) =>
         InlineOps.inlsubscriptf() 
     | VARexp (ref(VALvar{access=INLINE P.INLFUPDATEd, ...}),_) =>
         InlineOps.inlupdatef()
     | VARexp (ref(VALvar{access=INLINE(n), typ=ref ty,...}),_) => 
         let val t = transTyLty(ty)
         in PRIM(n,t) end
     | VARexp (ref(OVLDvar{name,...}),_) =>
	 impossible("unresolved overloading: "^Symbol.name name)
     | VARexp (ref ERRORvar,_) =>
         impossible ("error variable in translate")
     | APPexp (f,a) => 
         APP(translate loc f, translate loc a)
     | CONSTRAINTexp (e,_) => translate loc e
     | HANDLEexp (e,HANDLER(FNexp(l,ty))) =>
         let val ee = translate loc e
             val le = transrules loc l
             val lt = transTyLty ty 
             val re = MC.matchCompileHandler env (le, lt, err loc)
         in HANDLE(ee,re) end
     | RAISEexp(e,ty) => 
         let val t = transTyLty ty
          in RAISE(translate loc e,t)
         end
     | FNexp(l,ty) => 
         let val lt = transTyLty ty
             val le = transrules loc l
          in MC.matchCompile env (le, lt, err loc)
	 end
     | CASEexp (e,l) => 
         let val ee = translate loc e
             val le = transrules loc l
          in APP(MC.matchCompile env (le, BOGUSty, err loc),
		 ee)
	 end
     | LETexp (d,e) => makedec loc d (translate loc e)
     | x => ErrorMsg.impossibleWithBody "untranslateable expression"
	      (fn ppstrm =>
	       (add_string ppstrm "expression: ";
		PPAbsyn.ppExp (env,NONE) ppstrm (x,!printDepth)))

 in makedec (0,0) rootdec
end

end (* structure Translate *)
