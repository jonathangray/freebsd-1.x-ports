(* Copyright 1992 by AT&T Bell Laboratories *)
(***************************************************************************

	ELABCORE.SML translate AST to Absyn for the core language

 ***************************************************************************)

structure ElabCore: ELABCORE =
struct

open BasicTypes Symbol Absyn Ast PrintUtil AstUtil Types BasicTypes TyvarSet
     Modules EqTypes ModuleUtil TypesUtil Variables Misc ElabUtil ErrorMsg
     Access 

infix -->
type tyvUpdate = tyvarset -> unit
fun no_updt (_ : tyvarset) = ()

val union = union_tyvars

(**** TYPES ****)

fun elabTyv error (ln:linenum * linenum) (tyv:Ast.tyvar) =
  case tyv
  of Tyv vt => mkTyvar(mkUBOUND(vt, error ln))
   | MarkTyv(tyv,l1,l2) => elabTyv error (l1,l2) tyv

fun elabTyvList error ln tyvars =
  let val tvs = map (elabTyv error ln) tyvars
      val _ =  checkUniq ((error ln),"duplicate type variable name")
			 (map (fn (ref(ubound as OPEN{kind=UBOUND name,...}))
					=> name
				| _ => impossible "elabTyvList") tvs)
  in tvs end

type typeEnv = Modules.env * Normalize.env option

fun elabType error (ln:linenum * linenum) (env:typeEnv) (ast:Ast.ty) : 
      (Types.ty * tyvarset) =
  case ast
  of VarTy vt => 
       let val tyv = elabTyv error ln vt
       in (VARty tyv, singleton_tyvar tyv) end
   | ConTy (co,ts) => 
	let val co1 = 
	      if (Symbol.name (hd co)) = "->"
	      then BasicTypes.arrowTycon
	      else lookArTYC(env,co,length ts,error ln)
	    val (lts1,lvt1) = elabTypeList error ln env ts
	in (mkCONty (co1,lts1),lvt1) end
   | RecordTy lbs => 
       let val (lbs1,lvt1) = elabTLabel error ln env lbs
       in (recordTy(sortRecord(lbs1,error ln)),lvt1) end
   | TupleTy ts =>
	let val (lts1,lvt1) = elabTypeList error ln env ts
	in (tupleTy lts1,lvt1) end
   | MarkTy (ty,l1,l2) => elabType error (l1,l2) env ty

and elabTLabel error (ln:linenum * linenum) (env:typeEnv) labs =
  fold 
    (fn ((lb2,t2),(lts2,lvt2)) => 
	let val (t3,lvt3) = elabType error ln env t2
	in ((lb2,t3) :: lts2, union(lvt3,lvt2,error ln)) end)
    labs ([],no_tyvars)

and elabTypeList error (ln:linenum * linenum) (env:typeEnv) ts =
  fold 
    (fn (t2,(lts2,lvt2)) => 
	let val (t3,lvt3) = elabType error ln env t2
	in (t3 :: lts2, union(lvt3,lvt2,error ln)) end)
    ts ([],no_tyvars)
;

(**** TYPE DECLARATION ****)

fun elabTB error (ln:linenum * linenum) 
	   (ctx as (env:Modules.env,path:symbol list))
	   (notwith:bool) (tb : Ast.tb) : (Absyn.tb list * Modules.env) =
  case tb
  of Tb{tyc,def,tyvars} =>
	let val tvs = elabTyvList error ln tyvars
	    val ty = elabType error ln (env,NONE) def
	in makeTB(tvs,tyc,ty,error ln) notwith ctx end
   | MarkTb (tb,l1,l2) =>
	elabTB error (l1,l2) ctx notwith tb

fun elabTBlist (error,ln) (ctx as (env:Modules.env,path:symbol list)) 
      (notwith:bool) (tbl:Ast.tb list) : (Absyn.tb list * Modules.env) =
  revfold 
    (fn (typb2,(tbs2,env2)) =>
       let val (tb3,env3) =
	 elabTB error ln (Env.atop (env2,env),path) notwith typb2
       in (tb3 @ tbs2, Env.atop(env3,env2)) end)
    tbl ([],Env.empty)

fun elabTYPEdec
      (error,ln)
      (ctx as (env:Modules.env,path:symbol list,st:Stamps.scope))
      (tbl : Ast.tb list): (Absyn.dec * Modules.env * tyvarset * tyvUpdate) =
  let val tbenv1 = elabTBlist (error,ln) (env,path) true tbl
      val (dec4,env4) = makeTYPEdec (tbenv1,error ln)
  in (dec4,env4,no_tyvars,no_updt) end

(**** DATATYPE DECLARATION ****)
fun elabConstr error ln env rhs (name,SOME ty) =
      let val (t,tv) = elabType error ln env ty
      in ((name,false,(t --> rhs)),tv) end
  | elabConstr error ln env rhs (name,NONE) = ((name,true,rhs),no_tyvars)

fun elabDB (envorig,env,path,error) (args,name,def,ln) =
  let val rhs = mkCONty(lookArTYC (env,[name],length args,error ln),
		      map VARty args)
      val arity = length args
      val (dcl,tvs) = 
        fold
	  (fn (d,(dcl1,tvs1)) =>
	     let val (dc2,tv2) = elabConstr error ln env rhs d
	     in (dc2::dcl1,union(tv2,tvs1,error ln)) end)
	  def ([],no_tyvars)
      val _ = (checkbound(tvs,args,error ln); TypesUtil.bindTyvars args)
      val sdcl = sort3 dcl
      val sign = ConRep.boxed sdcl
      fun binddcons ((sym,const,typ),rep) =
	    let val _ = compressTy typ
		val typ = 
		  if arity > 0 then POLYty {sign=mkPolySign arity,abs=0,
					    tyfun=TYFUN{arity=arity,body=typ}}
		  else typ
	    in DATACON{name=sym, const=const, rep=rep, sign=sign, typ=typ} end
      fun binddconslist ((r1 as (name,_,_))::l1,r2::l2) =
	    let val dcon = binddcons (r1,r2)
		val (dcl,e2) = binddconslist (l1,l2)
	    in (dcon::dcl,Env.bind(name,CONbind dcon,e2)) end
	| binddconslist ([],[]) = ([],envorig)
	| binddconslist _ = impossible "elabDB.binddcons"
  in if length sdcl < length dcl  (* duplicate constructor names *)
     then let fun member(x:string,[]) = false
	        | member(x,y::r) = (x = y) orelse member(x,r)
	      fun dups([],l) = l
	        | dups(x::r,l) =
		    if member(x,r) andalso not(member(x,l))
		    then dups(r,x::l)
		    else dups(r,l)
	      fun add_commas [] = []
		| add_commas (y as [_]) = y
		| add_commas (s::r) = s :: "," :: add_commas(r)
	      val duplicates = dups(map (fn (n,_,_) => Symbol.name n) dcl,[])
	  in  error ln COMPLAIN (implode["datatype ", Symbol.name name,
					 " has duplicate constructor name(s): ",
					 implode(add_commas(duplicates))])
	        nullErrorBody
	  end
     else ();
     binddconslist(sdcl,sign)
  end

fun elabDATATYPEdec (error,ln) (env,path,scope:Stamps.scope) (db,tb) =
  let (* predefine datatypes *)
      fun predefine ln (Db{tyc=id,def,tyvars}) = 
	    let val r = ref(DATAtyc nil)
		val tyc = 
		  GENtyc{path=id::path,arity=length tyvars, eq=ref DATA,
			 stamp=Stamps.newStamp scope (), kind = r}
		val tvs = elabTyvList error ln tyvars
	    in (tvs,id,tyc,def,r,ln) end
	| predefine ln (MarkDb(db,l1,l2)) = predefine (l1,l2) db

      val (env1,db1,datatycs,datatyc_names,datacon_names) = 
        revfold 
          (fn (db2,(env2,dbs2,dts2,dtns2,dcns2)) => 
             let val (tvs,id3,tyc3,def3,r3,ln3) = predefine ln db2
	      in (Env.bind(id3,TYCbind tyc3,env2),
		  ((tvs,id3,def3,ln3),r3)::dbs2, tyc3::dts2, id3::dtns2,
		   (map #1 def3) @ dcns2)
	     end)
	  db (Env.empty,[],[],[],[])

      (* define types associated *)
      val (withtycs,env4) = 
	elabTBlist (error,ln) (Env.atop(env1,env),path) false tb

      (* builds the resulting environment *)
      val env5 = Env.atop(env4, env1)
      val fullEnv = (Env.atop(env5,env),NONE)

      (* look at the definition of datatypes *)
      fun redefine ((db,r),env6) = 
	let val (dcons,env7) = elabDB (env6,fullEnv,path,error) db 
	in r := DATAtyc(dcons); env7 end
      val env8 = revfold redefine db1 env5
      val withtyc_names =
	map (fn TB{tyc=DEFtyc{path=name::_,...},...} => name
	      | _ => ErrorMsg.impossible "CoreLang.elabDATATYPEdec")
	    withtycs
      val dtdec = DATATYPEdec{datatycs=rev datatycs,withtycs=rev withtycs}
  in checkUniq (error ln, "duplicate type names in type declaration")
	       (datatyc_names @ withtyc_names);
     checkUniq (error ln, "duplicate datacon names in datatype declaration")
               datacon_names;
     app (defineEqTycon (fn x => x)) datatycs;
     (dtdec,env8,no_tyvars,no_updt)
  end

(**** EXCEPTION DECLARATION ****)

fun elabEb error (ln:linenum * linenum) (env:Modules.env) (eb:Ast.eb) =
  case eb
  of EbGen{exn=id,etype=NONE} =>
       let val exn = DATACON{name=id,const=true,typ=exnTy,
			     rep=VARIABLEc(PATH[namedLvar id]), sign=[]}
       in ([EBgen{exn=exn,etype=NONE,ident=STRINGexp(Symbol.name id)}], 
	   Env.bind(id, CONbind exn, Env.empty),no_tyvars) end
   | EbGen{exn=id,etype=SOME typ} =>
       let val (ty,vt) = elabType error ln (env,NONE) typ
           val exn = DATACON{name=id,const=false,typ=(ty --> exnTy),
		     rep=VARIABLE(PATH[namedLvar id]), sign=[]}
       in
       ([EBgen{exn=exn,etype=SOME ty,ident=STRINGexp(Symbol.name id)}],
	Env.bind(id,CONbind exn, Env.empty),vt) 
       end
   | EbDef{exn=id,edef=qid} =>
       let val edef as DATACON{const,typ,rep,sign,...} =
	     lookEXN(env,qid,error ln)
	   val exn = DATACON{name=id,const=const,typ=typ,sign=sign,
			     rep=if const then VARIABLEc(PATH[namedLvar id])
				 else VARIABLE(PATH[namedLvar id])}
       in ([EBdef{exn=exn,edef=edef}],
	   Env.bind(id,CONbind exn,Env.empty),no_tyvars) end
   | MarkEb(eb,l1,l2) => elabEb error (l1,l2) env eb

fun elabEXCEPTIONdec (error,ln) (env: Modules.env) (excbinds:Ast.eb list) =
  let val (ebs,env,vt) = 
	revfold
	  (fn (exc1,(ebs1,env1,vt1)) =>
	     let val (eb2,env2,vt2) = elabEb error ln env exc1
	     in (eb2@ebs1,Env.atop(env2,env1),union(vt1,vt2,error ln)) end)
           excbinds ([],Env.empty,no_tyvars)
      fun getname(EBgen{exn=DATACON{name,...},...}) = name
        | getname(EBdef{exn=DATACON{name,...},...}) = name
  in 
  checkUniq (error ln, "duplicate exception declaration") (map getname ebs);
 (EXCEPTIONdec (rev ebs),env,vt,no_updt)
  end

(**** PATTERNS ****)

fun elabPat error (ln:linenum * linenum) (env:Modules.env) 
	    (pat:Ast.pat) : Absyn.pat * tyvarset =
  case pat
  of WildPat => (WILDpat,no_tyvars)
   | VarPat  path => 
       (clean_pat (error ln) (pat_id (path,env,error ln)),no_tyvars)
   | IntPat  n => (INTpat n,no_tyvars)
   | RealPat r => (REALpat r,no_tyvars)
   | StringPat s => (STRINGpat s,no_tyvars)
   | RecordPat {def,flexibility} =>
	let val (lps,tyv) = elabPLabel error ln env def
	in (makeRECORDpat (lps,flexibility,error ln),tyv) end
   | TuplePat pats =>
	let val (ps,tyv) = elabPatList error ln env pats
	in (TUPLEpat ps,tyv) end
   | VectorPat pats =>
	let val (ps,tyv) = elabPatList error ln env pats
	in (VECTORpat(ps,UNDEFty),tyv) end
   | AppPat {constr=path,argument=pat} =>
	let val dcb = pat_id (path,env,error ln)
	    val (p,tv) = elabPat error ln env pat
	in (makeAPPpat (error ln) (dcb,p),tv) end
   | ConstraintPat {pattern=pat,constraint=ty} =>
	let val (p1,tv1) = elabPat error ln env pat
	    val (t2,tv2) = elabType error ln (env,NONE) ty
	in (CONSTRAINTpat(p1,t2), union(tv1,tv2,error ln)) end
   | LayeredPat {varPat,expPat} =>
	let val (p1,tv1) = elabPat error ln env varPat
	    val (p2,tv2) = elabPat error ln env expPat
	in (makeLAYEREDpat(p1,p2,error ln),union(tv1,tv2,error ln)) end
   | MarkPat (pat,l1,l2) =>
	let val (p,tv) = elabPat error (l1,l2) env pat
	in (p,tv) end

and elabPLabel error (ln:linenum * linenum) (env:Modules.env) labs =
  revfold 
    (fn ((lb1,p1),(lps1,lvt1)) => 
	let val (p2,lvt2) = elabPat error ln env p1
	in ((lb1,p2) :: lps1, union(lvt2,lvt1,error ln)) end)
    labs ([],no_tyvars)

and elabPatList error (ln:linenum * linenum) (env:Modules.env) ps =
  fold 
    (fn (p1,(lps1,lvt1)) => 
	let val (p2,lvt2) = elabPat error ln env p1
	in (p2 :: lps1, union(lvt2,lvt1,error ln)) end)
    ps ([],no_tyvars)
;

(**** EXPRESSIONS ****)

fun elabExp (error,errorMatch) (ln:linenum * linenum) 
	    (ctx as (env:Modules.env,st:Stamps.scope))
	    (exp:Ast.exp) : (Absyn.exp * tyvarset * tyvUpdate) =
  case exp
  of VarExp path => (varcon(lookVARCON(env,path,error ln)),no_tyvars,no_updt)
   | IntExp n => (INTexp n,no_tyvars,no_updt)
   | RealExp r => (REALexp r,no_tyvars,no_updt)
   | StringExp s => (STRINGexp s,no_tyvars,no_updt)
   | RecordExp cells => 
	let val (les,tyv,updt) = elabELabel (error,errorMatch) ln ctx cells
	in (makeRECORDexp (les,error ln),tyv,updt) end
   | SeqExp exps =>
	let val (es,tyv,updt) = elabExpList (error,errorMatch) ln ctx exps
	in (SEQexp es,tyv,updt) end
   | TupleExp exps =>
	let val (es,tyv,updt) = elabExpList (error,errorMatch) ln ctx exps
	in (TUPLEexp es,tyv,updt) end
   | VectorExp exps =>
	let val (es,tyv,updt) = elabExpList (error,errorMatch) ln ctx exps
	in (VECTORexp es,tyv,updt) end
   | AppExp {function,argument} =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx function and
		(e2,tv2,updt2) = elabExp (error,errorMatch) ln ctx argument
	    fun updt tv = (updt1 tv;updt2 tv)
	in (APPexp (e1,e2),union(tv1,tv2,error ln),updt) end
   | ConstraintExp {expr=exp,constraint=ty} =>
	let val (e1,tv1,updt) = elabExp (error,errorMatch) ln ctx exp
	    val (t2,tv2) = elabType error ln (env,NONE) ty
	in (CONSTRAINTexp(e1,t2), union(tv1,tv2,error ln),updt) end
   | HandleExp {expr,rules} =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx expr
	    val (rls2,tv2,updt2) = elabMatch (error,errorMatch) ln ctx rules
	    fun updt tv = (updt1 tv;updt2 tv)
	in (makeHANDLEexp (e1, rls2), union(tv1,tv2,error ln),updt) end
   | RaiseExp exp =>
	let val (e,tyv,updt) = elabExp (error,errorMatch) ln ctx exp
	in (RAISEexp(e,UNDEFty),tyv,updt) end
   | LetExp {dec,expr} => 
	let val (d1,e1,tv1,updt1) =
		   elabDec (error,errorMatch,ln) (env,[],st) dec
	    val (e2,tv2,updt2) = elabExp (error,errorMatch) ln (Env.atop(e1,env),st) expr
	    fun updt tv = (updt1 tv;updt2 tv)
	in (LETexp(d1,e2), union(tv1,tv2,error ln),updt) end
   | CaseExp {expr,rules} =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx expr
	    val (rls2,tv2,updt2) = elabMatch (error,errorMatch) ln ctx rules
	    fun updt tv = (updt1 tv;updt2 tv)
	in (CASEexp (e1,completeMatch (errorMatch ln) rls2),
	    union(tv1,tv2,error ln),updt) end
   | IfExp {test,thenCase,elseCase} =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx test and
		(e2,tv2,updt2) = elabExp (error,errorMatch) ln ctx thenCase and
		(e3,tv3,updt3) = elabExp (error,errorMatch) ln ctx elseCase
	    fun updt tv = (updt1 tv;updt2 tv;updt3 tv)
	in (IFexp(e1,e2,e3), union(tv1,union(tv2,tv3,error ln),error ln),updt) end
   | AndalsoExp (exp1,exp2) =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx exp1 and 
		(e2,tv2,updt2) = elabExp (error,errorMatch) ln ctx exp2
	    fun updt tv = (updt1 tv;updt2 tv)
	in (IFexp(e1, e2, FALSEexp), union(tv1,tv2,error ln),updt) end
   | OrelseExp (exp1,exp2) =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx exp1 and 
		(e2,tv2,updt2) = elabExp (error,errorMatch) ln ctx exp2
	    fun updt tv = (updt1 tv;updt2 tv)
	in (IFexp(e1 ,TRUEexp, e2), union(tv1,tv2,error ln),updt) end
   | WhileExp {test,expr} =>
	let val (e1,tv1,updt1) = elabExp (error,errorMatch) ln ctx test and 
		(e2,tv2,updt2) = elabExp (error,errorMatch) ln ctx expr
	    fun updt tv = (updt1 tv;updt2 tv)
	in (WHILEexp(e1,e2), union(tv1,tv2,error ln), updt) end
   | FnExp rules => 
	let val (rls,tyv,updt) = elabMatch (error,errorMatch) ln ctx rules
	in (FNexp (completeMatch (errorMatch ln) rls,UNDEFty),tyv,updt) end
   | MarkExp (exp,l1,l2) => 
	let val (e,tyv,updt) = elabExp (error,errorMatch) (l1,l2) ctx exp
	in 
	(if !System.Control.markabsyn then MARKexp(e,l1,l2) else e,tyv,updt)
	end
   | SelectorExp s => (SELECTORexp s,no_tyvars,no_updt)

and elabELabel (error,errorMatch) ln ctx labs =
  let val (les1,lvt1,updt1) =
	fold 
	  (fn ((lb2,e2),(les2,lvt2,updts2)) => 
	      let val (e3,lvt3,updt3) = elabExp (error,errorMatch) ln ctx e2
	      in ((lb2,e3) :: les2, union(lvt3,lvt2,error ln), updt3 :: updts2) end)
	  labs ([],no_tyvars,[])
      fun updt tv : unit = app (fn f => f tv) updt1
  in (les1, lvt1, updt) end

and elabExpList (error,errorMatch) ln ctx es =
  let val (les1,lvt1,updt1) =
	fold 
	  (fn (e2,(es2,lvt2,updts2)) => 
	      let val (e3,lvt3,updt3) = elabExp (error,errorMatch) ln ctx e2
	      in (e3 :: es2, union(lvt3,lvt2,error ln), updt3 :: updts2) end)
	  es ([],no_tyvars,[])
      fun updt tv: unit = app (fn f => f tv) updt1
  in (les1, lvt1, updt) end

and elabMatch (error,errorMatch) ln ctx rs = 
  let val (rs,lvt,updt1) =
	fold 
	  (fn (r1,(rs1,lvt1,updt1)) => 
	      let val (r2,lvt2,updt2) = elabRule (error,errorMatch) ln ctx r1
	      in (r2 :: rs1, union(lvt2,lvt1,error ln), updt2::updt1) end)
	  rs ([],no_tyvars,[])
      fun updt tv: unit = app (fn f => f tv) updt1
  in (rs, lvt, updt) end
and elabRule (error,errorMatch) ln (env,st) (Rule{pat,exp}) =
  let val ln' = case pat of MarkPat (p,l1,l2) => (l1,l2) | _ => ln
      val (p,tv1) = elabPat error ln env pat
      val env' = Env.atop(bindVARp ([p],error ln'), env)
      val (e,tv2,updt) = elabExp (error,errorMatch) ln (env',st) exp
  in (RULE(p,e),union(tv1,tv2,error ln),updt) end

(**** ELABORATE SIMPLE DECLARATIONS ****)

and elabDec (error,errorMatch,ln) 
	    (ctx as (env,path,scope:Stamps.scope)) (dec:Ast.dec) : 
	(Absyn.dec * Modules.env * tyvarset * tyvUpdate) =
  case dec 
  of TypeDec tbs => 
	elabTYPEdec (error,ln) ctx tbs
   | DatatypeDec{datatycs,withtycs} => 
	elabDATATYPEdec (error,ln) ctx (datatycs,withtycs)
   | AbstypeDec{abstycs,withtycs,body} =>
	elabABSTYPEdec (error,errorMatch,ln) ctx (abstycs,withtycs,body)
   | ExceptionDec ebs =>
	elabEXCEPTIONdec (error,ln) env ebs
   | ValDec vbs =>
	elabVALdec (error,errorMatch,ln) ctx vbs
   | FunDec fbs =>
	elabFUNdec (error,errorMatch,ln) ctx fbs
   | ValrecDec rvbs =>
	elabVALRECdec (error,errorMatch,ln) ctx rvbs
   | SeqDec ds =>
	elabSEQdec (error,errorMatch,ln) ctx ds
   | LocalDec ld => elabLOCALdec (error,errorMatch,ln) ctx ld
   | OpenDec ds => elabOPENdec (error,ln) ctx ds
   | FixDec (ds as {fixity,ops}) => 
	let val env = 
          fold (fn (id,env) =>
		  Env.bind(id,FIXbind(FIXvar{name=id,binding=fixity}),env))
	       ops Env.empty
	in (FIXdec ds,env,no_tyvars,no_updt) end
   | OvldDec dec  => elabOVERLOADdec (error,errorMatch,ln) ctx dec
   | MarkDec(dec,l1,l2) =>
        let val (d,env,tv,updt)= elabDec (error,errorMatch,(l1,l2)) ctx dec
        in 
	(if !System.Control.markabsyn then MARKdec(d,l1,l2) else d,env,tv,updt)
	end
   | StrDec _ => impossible "strdec"
   | AbsDec _ => impossible "absdec"
   | ImportDec _ => impossible "importdec"
   | FctDec _ => impossible "fctdec"
   | SigDec _ => impossible "sigdec"
   | FsigDec _ => impossible "fsigdec"
	
(**** ABSTRACT TYPES DECLARATION ****)

and elabABSTYPEdec (error,errorMatch,ln) (ctx as (env,path,scope:Stamps.scope))
		   (db,tb,ldecs) =
    let val (dtv,env0,_,_) =
	  elabDATATYPEdec (error,ln) ctx (db,tb)
	val (DATATYPEdec{datatycs,withtycs}) = dtv
        val withtycons = map (fn TB{tyc,...} => tyc) withtycs 
        val (body,env'',tv,updt) = 
	  elabDec (error,errorMatch,ln) (Env.atop(env0,env),path,scope) ldecs
	fun bind (tyc,env') = Env.bind(tycName tyc,TYCbind tyc,env')
	(* will become abstycs during type checking *)
	val envdt = fold bind datatycs env''
	val envwt = fold bind withtycons envdt (* withtycs *)
    in (ABSTYPEdec{abstycs=datatycs,withtycs=withtycs,body=body},
	envwt,tv,updt) end

(**** OVERLOADING ****)

and elabOVERLOADdec (error,errorMatch,ln) (env,path,stamps) (id,typ,exps)=
    let val (body,tyvars) = elabType error ln (env,NONE) typ 
        val tvs = get_tyvars tyvars
        val scheme = (TypesUtil.bindTyvars tvs; TypesUtil.compressTy body;
		      TYFUN{arity=length tvs, body=body})
        fun option (MARKexp(e,_,_)) = option e
	  | option (VARexp(ref (v as VALvar{typ,...}),_)) =
              {indicator = TypesUtil.matchScheme(scheme,!typ), variant = v}
          | option _ = ErrorMsg.impossible "CoreLang.makeOVERLOADdec.option"
	val exps = map (elabExp (error,errorMatch) ln (env,stamps)) exps
	val exps1 = map #1 exps and exps3 = map #3 exps
	fun updt tv: unit = app (fn f => f tv) exps3
	val ovldvar = 
	  OVLDvar{name=id,scheme=scheme,
		  options=ref(map option exps1)}
    in (OVLDdec ovldvar, Env.bind(id,VARbind ovldvar,Env.empty),no_tyvars,updt)
    end

and elabLOCALdec (error,errorMatch,ln) (env,path,st) (ldecs1,ldecs2)=
  let val (ld1,env1,tv1,updt1) = 
	elabDec (error,errorMatch,ln) (env,[],st) ldecs1
      val (ld2,env2,tv2,updt2) = 
	elabDec (error,errorMatch,ln) (Env.atop(env1,env),path,st) ldecs2
      fun updt tv = (updt1 tv;updt2 tv)
  in (LOCALdec(ld1,ld2), env2,union(tv1,tv2,error ln),updt)
  end

and elabOPENdec (error,ln) (env,_,_) qid_p = 
   let val strs = map (fn s => lookSTR(env,s,error ln)) qid_p
       fun openit (str,env) = openStructureVar (env,str)
    in (OPENdec strs, revfold openit strs Env.empty, no_tyvars,no_updt)
   end

(****  VALUE DECLARATION ****)

and elabVB (error,errorMatch,_)  ctx (MarkVb(vb,l1,l2)) = 
      elabVB (error,errorMatch,(l1,l2)) ctx vb
  | elabVB (error,errorMatch,ln) (env,st) (Vb{pat,exp}) =
  let val (pat,pv) = elabPat error ln env pat
      val (exp,ev,updtexp) = elabExp (error,errorMatch) ln (env,st) exp
      fun stripped (MARKexp(e,_,_)) = stripped e
	| stripped (CONSTRAINTexp(e,_)) = stripped e
	| stripped e = e
      val tvref = ref []
      fun updt tv: unit =
        let val localtyvars = diff_tyvars(union_tyvars(pv,ev,error ln),tv,error ln)
	    val localtyvarlist = get_tyvars localtyvars
	    val downtyvars = union_tyvars(localtyvars,tv,error ln)
        in tvref := localtyvarlist; updtexp downtyvars end
      val pat = 
	case stripped exp
	of VARexp(ref(VALvar{access as INLINE _,...}),_) =>
	     (case pat
	      of CONSTRAINTpat(VARpat(VALvar{name,typ,...}), ty) =>
		   CONSTRAINTpat(VARpat(VALvar{name=name,typ=typ,
					       access=access}),ty)
	       | VARpat(VALvar{name, typ,...}) =>
		   VARpat(VALvar{name=name,typ=typ,access=access})
	       | CONSTRAINTpat(WILDpat, ty) => CONSTRAINTpat(WILDpat, ty)
	       | WILDpat => WILDpat
	       | _ => impossible "elabVB")
	 | _ => pat
  in 
  (VB{exp=exp, tyvars=tvref, pat=pat},updt)
  end

and elabVALdec (error,errorMatch,ln) (env,path,st) vb =
  let val (ds,updt1) = 
	fold 
	  (fn (vdec,(ds1,updt1)) => 
	     let val (d2,updt2) = elabVB (error,errorMatch,ln) (env,st) vdec
	     in (d2::ds1,updt2::updt1) end)
	  vb ([],[])
      fun updt tv : unit = app (fn f => f tv) updt1
  in (VALdec ds, bindVARp (map (fn VB{pat,...}=>pat) ds,error ln),
      no_tyvars,updt) end

and elabRVB (error,errorMatch,_) ctx (MarkRvb(rvb,l1,l2)) =
      elabRVB (error,errorMatch,(l1,l2)) ctx rvb
  | elabRVB (error,errorMatch,ln) (env,st) (Rvb{var,exp,resultty}) =
  let val (e,ev,updt) = elabExp (error,errorMatch) ln (env,st) exp
      val (t,tv) = 
	case resultty 
	of SOME t1 => 
	     let val (t2,tv2) = elabType error ln (env,NONE) t1
	     in (SOME t2,tv2) end
	 | NONE => (NONE,no_tyvars)
  in ({match = e , ty = t, name=var},union(ev,tv,error ln),updt) end

and elabVALRECdec  (error,errorMatch,ln) (ctx as (env,path,st)) rvb =
  let 
      val env' = ref(Env.empty: Modules.env)
      fun makevar (p as Rvb{var,exp,resultty}) =
	    let val v = mkVALvar var
	    in env' := Env.bind(var,VARbind v,!env'); (v,p) end
	| makevar (p as MarkRvb(rvb,_,_)) = 
	    let val (v,_) = makevar rvb in (v,p) end
      val rvbs' = map makevar rvb
      val env'' = Env.atop(!env', env)
      val new_ctx = (env'',st)
      val (rvbs,tyvars,updt)=
	revfold (fn((v,rvb1),(rvbs1,tvs1,updt1))=>
		   let val (rvb2,tv2,updt2) =
			 elabRVB (error,errorMatch,ln) new_ctx rvb1
		   in ((v,rvb2)::rvbs1, 
			union_tyvars(tv2,tvs1,error ln),
			updt2::updt1)
		   end) 
		rvbs' ([],no_tyvars,[])
      fun updtexp tv : unit = app (fn f => f tv) updt
      val tvref = ref []
      fun updt tv : unit =
        let val downtyvars = union(tyvars,tv,error ln)
            val localtyvarlist = get_tyvars(diff_tyvars(tyvars,tv,error ln))
	in tvref := localtyvarlist; updtexp downtyvars end
  in
  checkUniq (error ln,"duplicate function name in val rec dec")
	    (map (fn (v,{name,...}) => name) rvbs);
  (VALRECdec (map (fn (v,{ty,match,name}) =>
		      RVB{var=v,resultty=ty,tyvars=tvref, exp=match})
                  rvbs),
   !env',no_tyvars,updt)
  end

and elabFUNdec (error,errorMatch,ln) (env,_,st) fb =
  let fun makevar ln (MarkFb(fb,l1,l2),ctx) = makevar (l1,l2) (fb,ctx)
	| makevar ln (Fb{var,clauses},(lcl,env')) =
	   let val v = mkVALvar var
	       val _ = 
		 case clauses
		 of (Clause{pats,...})::rest => 
		      let val len = length pats
		      in 
	              if exists (fn (Clause{pats,...}) => len <> length pats)
				rest
	              then error ln COMPLAIN 
			     "clauses don't all have same number of patterns"
			     nullErrorBody
	              else ()
		      end
		  | [] => impossible "elabFUNdec"
	   in ((v,clauses,ln)::lcl,Env.bind(var,VARbind v,env')) end
      val (clauses,env') = revfold (makevar ln) fb ([],Env.empty)
      val env'' = Env.atop(env',env)
      fun makeclause ln (Clause{pats,resultty,exp}) =
	let val (pats,tv1) = elabPatList error ln env pats
	    val (exp,tv2,updt) = 
	      elabExp (error,errorMatch) ln 
		      (Env.atop(bindVARp(pats,error ln),env''),st) exp
	    val (ty,tv3) = case resultty
	      of NONE => (NONE,no_tyvars)
	       | SOME t => 
		   let val (t4,tv4) = elabType error ln (env,NONE) t
		   in (SOME t4,tv4) end
	in 
	(CLAUSE{pats=pats,resultty=ty,exp=exp},
		union(tv1,union(tv2,tv3,error ln),error ln),updt) end
      fun evalclauses ((var,clauses,ln),(fs,tvs,updt)) = 
            let val (cs1,tvs1,updt1) =
              revfold (fn (c2,(cs2,tvs2,updt2)) =>
			 let val (c3,tvs3,updt3) = makeclause ln c2
			 in (c3::cs2,union(tvs3,tvs2,error ln),updt3::updt2) end) 
		      clauses ([],no_tyvars,[])
            in ((var,rev cs1)::fs,union(tvs1,tvs,error ln),updt1 @ updt) end
      val (fbs1,fv,updt1) = revfold evalclauses clauses ([],no_tyvars,[])
      val tvref = ref []
      fun updt tv : unit =  
        let val localtyvars = diff_tyvars(fv,tv,error ln)
	    val downtyvars = union_tyvars(localtyvars,tv,error ln)
	    val localtyvarlist = get_tyvars localtyvars
	in tvref := localtyvarlist; app (fn f => f downtyvars) updt1 end
      fun makefb (v as VALvar{name=[n],...},cs) =
	    (FB{var=v,clauses=cs, tyvars=tvref})
	| makefb _ = ErrorMsg.impossible "makeFUNdec.makefb"
  in checkUniq  (error ln,"duplicate function names in fun dec") 
		(map (fn(VALvar{name=[n],...},_)=>n
			| _ => impossible "makeFUNdec:checkuniq") fbs1);
   (FUNdec(map makefb fbs1,errorMatch,ln), env',no_tyvars, updt)
  end

and elabSEQdec (error,errorMatch,ln) (ctx as (env,path,st)) ds =
  let val (ds1,env1,tv1,updt1) = 
	revfold 
	 (fn (decl2,(ds2,env2,tvs2,updt2)) =>
	    let val (d3,env3,tvs3,updt3) =
		     elabDec (error,errorMatch,ln)
			     (Env.atop(env2,env),path,st) decl2
	     in (d3::ds2,Env.atop(env3,env2),union(tvs3,tvs2,error ln),updt3::updt2)
	    end)
	 ds ([],Env.empty,no_tyvars,[])
      fun updt tv : unit = app (fn f => f tv) updt1
  in (SEQdec(rev ds1),env1,tv1,updt) end


end (* structure ElabCore *)
