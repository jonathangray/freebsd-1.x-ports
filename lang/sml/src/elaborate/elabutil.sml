(* Copyright 1992 by AT&T Bell Laboratories *)

structure ElabUtil:ELABUTIL = struct
local
  open Symbol Absyn Ast ErrorMsg PrintUtil AstUtil Types BasicTypes TyvarSet
  Access Modules EqTypes ModuleUtil TypesUtil Variables Misc
in

val unitPat = RECORDpat{fields = nil, flex = false, typ = ref UNDEFty,
                        pats = ref nil}
			
val unitExp = RECORDexp nil

val TRUEpat = CONpat(trueDcon,NONE)
val TRUEexp = CONexp(trueDcon,NONE)
val FALSEpat = CONpat(falseDcon,NONE)
val FALSEexp = CONexp(falseDcon,NONE)



(* These constants are never used; NONE may not be appropriate here (zsh) *)
val NILpat = CONpat(nilDcon,NONE)
val NILexp = CONexp(nilDcon,NONE)
val CONSpat = fn pat => APPpat(consDcon,NONE,pat)
val CONSexp = CONexp(consDcon,NONE)

(* Verifies that all the elements of a list are unic *)
fun checkUniq (err,message) l =
 let val l' = Sort.sort Symbol.symbolGt l
     fun f (x::y::rest) = (
	   if Symbol.eq(x,y)
	   then err COMPLAIN (message^ ": " ^ Symbol.name x) nullErrorBody
	   else ();
	   f(y::rest))
       | f _ = ()
 in f l'
 end

(* extract all the variables from a pattern *)
fun bindVARp (patlist,err) =
 let val vl = ref (nil: symbol list)
     val env = ref(Env.empty: Modules.env)
     val rec f =
           fn VARpat(v as VALvar{name=[name],...})=> 
		    (if Symbol.eq(name,EQUALsym)
		     then err WARN "rebinding =" nullErrorBody
		     else ();
		     env := Env.bind(name,VARbind v,!env); 
	 	     vl := name :: !vl)
	    | RECORDpat{fields,...} => app(fn(_,pat)=>f pat) fields
            | VECTORpat(pats,_) => app f pats
	    | APPpat(_,_,pat) => f pat
	    | CONSTRAINTpat(pat,_) => f pat
	    | LAYEREDpat(p1,p2) => (f p1; f p2)
	    | _ => ()
 in app f patlist;
    checkUniq (err,"duplicate variable in pattern(s)") (!vl);
    !env
 end

(* sort the labels in a record the order is redefined to take the usual 
   ordering on numbers expressed by strings (tuples) *)

local 
  fun gtr((a,_),(b,_)) = 
    let val a' = Symbol.name a and b' = Symbol.name b
	val zero = ord "0" and nine = ord "9"
	val a0 = ordof(a',0) and b0 = ordof(b',0)
    in 
    if a0 >= zero andalso a0 <= nine
    then if b0 >= zero andalso b0 <= nine
	 then size a' > size b' orelse size a' = size b' andalso a' > b'
	 else false
    else if b0 >= zero andalso b0 <= nine then true else a' > b'
    end
  val sort = Sort.sort gtr
in fun sortRecord(l,err) =
     (checkUniq(err, "duplicate label in record") (map #1 l); sort l)
end

fun makeRECORDexp(fields,err) =
  let val fields' = map (fn(id,exp)=> (id,(exp,ref 0))) fields
      fun assign(i,(_,(_,r))::tl) = (r := i; assign(i+1,tl))
	| assign(_,nil) = ()
      fun f(i,(id,(exp,ref n))::r) = (LABEL{name=id,number=n},exp)::f(i+1,r)
        | f(_,nil) = nil
  in assign(0, sortRecord(fields',err)); RECORDexp(f(0,fields')) end

fun TUPLEexp l = 
  let fun addlabels(i,e::r) = 
	    (LABEL{number=i-1,name=(Tuples.numlabel i)},e) :: addlabels(i+1,r)
        | addlabels(_, nil) = nil
  in RECORDexp (addlabels(1,l)) end


(* Adds a default case to a list of rules. 
   If any member of given list is marked, all ordinarily-marked expressions 
     in default case are also marked, using last mark in given list 
     as location.
   KLUDGE! The debugger distinguishes marks in the default case by
     the fact that start and end locations for these marks 
     are the same! *)
fun completeMatch'' rule =
  let fun f _ ((r as RULE(pat,MARKexp(_,left,_)))::rest) =
             r :: (f (SOME left) rest)
	| f markloc (r::rest) = r :: (f markloc rest)
	| f (SOME loc) nil = [rule (fn exp => MARKexp(exp,loc,loc))]
	| f NONE nil = [rule (fn exp => exp)]
  in f NONE
  end

fun completeMatch' (RULE(p,e)) =
    completeMatch'' (fn marker => RULE(p,marker e))

fun completeMatch s  =
  completeMatch'' (fn marker =>
    RULE(WILDpat, 
	 marker(case CoreInfo.errorMatchPath
		of ref ERRORvar => 
		    RAISEexp(CONexp(!CoreInfo.exnMatch,NONE),UNDEFty)
		 | varError =>
		    SEQexp[APPexp(marker(VARexp(ref Prim.assignVar,NONE)),
				  TUPLEexp[VARexp (varError,NONE), 
					   STRINGexp s]),
			   marker(RAISEexp(CONexp(!CoreInfo.exnMatch,NONE),UNDEFty))])))
		       

fun SELECTORexp id = 
  let val v = mkVALvar id
  in FNexp(completeMatch "selector"
			 [RULE(RECORDpat{fields=[(id,VARpat v)], flex=true,
					typ= ref UNDEFty, pats=ref nil},
			 VARexp(ref v,NONE))],UNDEFty)
  end

(* Transform a while loop in a call to a recursive function *)
val whileSym = Symbol.varSymbol "while"

fun IFexp (a,b,c) =
    CASEexp(a, completeMatch "if" [RULE(TRUEpat,b), RULE(FALSEpat,c)])

fun TUPLEpat l =
  let fun addlabels(i,e::r) = (Tuples.numlabel i, e) :: addlabels(i+1, r)
	| addlabels(_, nil) = nil
  in
  RECORDpat{fields=addlabels(1,l), flex=false, typ=ref UNDEFty, pats=ref nil}
  end

val argName = [Symbol.varSymbol "arg"]

fun FUNdec (fbl,errorMatch,ln) =
    let fun fb2rvb (FB {var, clauses as (CLAUSE{pats,...}::_),tyvars}) =
	    let fun getvar _ =  VALvar{access=PATH[mkLvar()],name=argName,
				       typ=ref UNDEFty}
		val vars = map getvar pats
		fun not1(f,[a]) = a
		  | not1(f,l) = f l
		fun dovar valvar = VARexp(ref(valvar),NONE)
		fun doclause (CLAUSE{pats,exp,resultty=NONE}) =
			      RULE(not1(TUPLEpat,pats), exp)
		  | doclause (CLAUSE{pats,exp,resultty=SOME ty}) =
			      RULE(not1(TUPLEpat,pats),CONSTRAINTexp(exp,ty))

	        fun last[x] = x | last (a::r) = last r
		val mark =  case (hd clauses, last clauses)
	                     of (CLAUSE{exp=MARKexp(_,a,_),...},
				 CLAUSE{exp=MARKexp(_,_,b),...}) =>
			         (fn e => MARKexp(e,a,b))
			      | _ => fn e => e
		fun makeexp [var] = 
                      FNexp(completeMatch(errorMatch ln)
					 (map doclause clauses),UNDEFty)
		  | makeexp vars = 
                      fold (fn (w,e) => 
                             FNexp(completeMatch (errorMatch ln)
						 [RULE(VARpat w,mark e)],
                                   UNDEFty)) vars
				(CASEexp(TUPLEexp(map dovar vars),
					 completeMatch (errorMatch ln)
						       (map doclause clauses)))
	     in RVB {var=var,
		     exp=makeexp vars,
		     resultty=NONE,
		     tyvars=tyvars}
	    end
          | fb2rvb _ = ErrorMsg.impossible "absyn.38"
    in VALRECdec (map fb2rvb fbl) end

fun WHILEexp (a,b) =
  let val fvar = mkVALvar whileSym
      val id = fn x => x
      val (markdec,markall,markend,markbody) =
	case (a,b)
	of (MARKexp(_,a1,a2), MARKexp(_,b1,b2)) =>
	      (fn e => MARKdec(e,a1,b2), fn e => MARKexp(e,a1,b2),
	       fn e => MARKexp(e,b2,b2), fn e => MARKexp(e,b1,b2))
	 | _ => (id,id,id,id)
      val body = 
        markbody(SEQexp[b, APPexp(markend(VARexp(ref fvar,NONE)), 
                                  markend unitExp)])
      val loop = markall(IFexp(a,body, markend unitExp))
      val fnloop = markall(FNexp(completeMatch "while"
						[RULE(unitPat,loop)],UNDEFty))
  in 
  markall 
    (LETexp(
       markdec 
         (VALRECdec[RVB{var=fvar, exp=fnloop, resultty = NONE, 
			tyvars = ref nil}]),
       APPexp(markall(VARexp (ref fvar,NONE)), markend unitExp)))
  end

fun makeHANDLEexp(exp,rules) =
  let 
    val v = mkVALvar exnID
    val r = RULE(VARpat v, RAISEexp(VARexp(ref(v),NONE),UNDEFty))
    val rules = completeMatch' r rules 
  in 
    HANDLEexp(exp, HANDLER(FNexp(rules,UNDEFty))) 
  end

(* transforme a VarPat in either a variable or a constructor. If we are given
   a long path (>1) then it has to be a constructor *)

fun pat_id (qid,env,err) = 
  case qid
  of [id] =>
    ((case lookShortVARCON (env,id,fn _ => raise Env.Unbound)
     of CONbind c => CONpat(c,NONE) 
      | _ => VARpat(mkVALvar id))
     handle Env.Unbound => VARpat(mkVALvar id))
   | _ =>
    CONpat((case lookVARCON (env,qid,err)
             of VARbind c =>
		(err COMPLAIN 
		  (formatQid qid^" is a variable. It must be a constructor.")
		  nullErrorBody;
	         (bogusCON,NONE))
              | CONbind c => (c,NONE)
              | _ => ErrorMsg.impossible "CoreLang.qid_pat")
            handle Env.Unbound => impossible "unbound untrapped")

fun makeRECORDpat(l,flex,err) =
  RECORDpat{fields=sortRecord(l,err), flex=flex, typ=ref UNDEFty, pats=ref nil}

fun clean_pat err (CONpat(DATACON{const=false,name,...},_)) = 
      (err COMPLAIN ("data constructor "^Symbol.name name^
		     " used without argument in pattern")
         nullErrorBody;
       WILDpat)
  | clean_pat err p = p

fun spath_to_string [] = "<empty>"
  | spath_to_string [n] = Symbol.name n
  | spath_to_string (n::r) =
    implode(Symbol.name n :: fold (fn(n,b) => ("."::Symbol.name n::b)) r [])

fun pat_to_string WILDpat = "_"
  | pat_to_string (VARpat(VALvar{name,...})) = spath_to_string name
  | pat_to_string (CONpat(DATACON{name,...},_)) = Symbol.name name
  | pat_to_string (INTpat i) = makestring i
  | pat_to_string (REALpat s) = s
  | pat_to_string (STRINGpat s) = s
  | pat_to_string (RECORDpat _) = "<record>"
  | pat_to_string (APPpat _) = "<application>"
  | pat_to_string (CONSTRAINTpat _) = "<constraint pattern>"
  | pat_to_string (LAYEREDpat _) = "<layered pattern>"
  | pat_to_string (VECTORpat _) = "<vector pattern>"
  | pat_to_string (ORpat _) = "<or pattern>"
  | pat_to_string _ = "<illegal pattern>"

fun makeAPPpat err (CONpat(d as DATACON{const=false,...},t),p) = APPpat(d,t,p)
  | makeAPPpat err (CONpat(d as DATACON{name,...},_),_) = 
      (err COMPLAIN
        ("constant constructor applied to argument in pattern:"
	 ^ Symbol.name name)
         nullErrorBody;
       WILDpat)
  | makeAPPpat err (rator,_) = 
      (err COMPLAIN (implode["non-constructor applied to argument in pattern: ",
			     pat_to_string rator])
         nullErrorBody;
       WILDpat)

fun makeLAYEREDpat ((x as VARpat _), y, _) = LAYEREDpat(x,y)
  | makeLAYEREDpat (CONSTRAINTpat(x as VARpat _,t), y, _) = 
      LAYEREDpat(x,CONSTRAINTpat(y,t))
  | makeLAYEREDpat (x,y,err) =
      (err COMPLAIN "pattern to left of AS must be variable" nullErrorBody;
       y)

fun calc_strictness (arity, body) =
    let val argument_found = Array.array(arity,false)
	fun search(VARty(ref(INSTANTIATED ty))) = search ty
	  | search(IBOUND n) = Array.update(argument_found,n,true)
	  | search(CONty(tycon, args)) = app search args
	  | search _ = ()	(* for now... *)
    in
	search body;
	ArrayExt.listofarray argument_found
    end;

fun makeTB(args,name,(ty,tv),err) notwith (env,path) =
  let val _ = 
	(checkbound(tv,args,err); TypesUtil.bindTyvars args; compressTy ty)
      val arity = length args
      val binding = 
	DEFtyc{path=name::path, strict=calc_strictness(arity,ty),
	       tyfun=TYFUN{arity=arity, body=ty}}
  in ([TB{tyc=binding,def=ty}],
      if notwith then Env.empty else Env.bind(name,TYCbind binding,Env.empty))
  end

fun makeTYPEdec ((tbs,env),err) =
  let val _ =  
        checkUniq (err, "duplicate type definition") 
                  (map (fn TB{tyc=DEFtyc{path=name::_,...},...} => name
			 | _ => ErrorMsg.impossible "CoreLang.makeTYPEdec")
			tbs)
      val env' = ref env  
      fun bindtyc (TB{tyc as DEFtyc{path=name::_,...},...}) = 
	    env' := Env.bind(name,TYCbind tyc,!env')
        | bindtyc _ = ErrorMsg.impossible "makeTYPEdec.bindtyc"
  in app bindtyc tbs; (TYPEdec tbs, !env') end

end

end
