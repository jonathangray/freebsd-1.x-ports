(* Copyright 1992 by AT&T Bell Laboratories *)

structure AstUtil:ASTUTIL = struct    

local
  open Symbol Fixity Ast ErrorMsg Symbol PrintUtil Modules Fixity 
in

type parseEnv = Modules.env

val unitPat = RecordPat{def=nil,flexibility=false}
val unitExp = RecordExp nil
val trueDcon = [varSymbol "true"]
val falseDcon = [varSymbol "false"]
val nilDcon = [varSymbol "nil"]
val consDcon = [varSymbol "::"]
val quoteDcon = [varSymbol "QUOTE"]
val antiquoteDcon = [varSymbol "ANTIQUOTE"]
val truePat = VarPat(trueDcon)
val trueExp = VarExp(trueDcon)
val falsePat = VarPat(falseDcon)
val falseExp = VarExp(falseDcon)
val nilPat = VarPat(nilDcon)
val nilExp = VarExp(nilDcon)
val consPat = fn pat => AppPat{constr=consDcon,argument=pat}
val consExp = VarExp(consDcon)
val arrowTycon = tycSymbol "->"
val exnID = Symbol.tycSymbol "exn"
val bogusID = varSymbol "BOGUS"
val symArg = strSymbol "<Parameter>"
val itsym = [varSymbol "it"]


(* list expression *)
fun ListExp l =
  fold (fn (e,rest) => AppExp{function=consExp,argument=TupleExp[e,rest]})
       l nilExp

(* list pattern *)
fun ListPat l =
  fold (fn (e,rest) => AppPat{constr=consDcon, argument=TuplePat[e,rest]})
       l nilPat

(* THE PRECEDENCE PARSER *)


(* THE PRECEDENCE PARSER *)

(* internal type for the precedence stack *)
abstype 'a precStack = INf of int * 'a * 'a precStack
		   | NONf of 'a * 'a precStack
		   | NILf
with 

fun precedence (app,pair) =
  
 let (* verifies it is non fix *)
     fun ensure_nonfix (e,NONfix,_) = e
       | ensure_nonfix (e,INfix _,err) = 
	  (err COMPLAIN "nonfix identifier required" nullErrorBody; e)

     (* starts the parser *)
     fun start(e,f,err) = NONf(ensure_nonfix(e,f,err), NILf)

     (* parse an expression *)
     fun parse(NONf(e,r), e',NONfix,err) = NONf(app err(e,e'),r)
       | parse(p as INf _, x,f,err) = NONf(ensure_nonfix(x,f,err), p)
       | parse(p as NILf, _,_,err) = impossible "Corelang.parse NILf"
       | parse(p as NONf(e1,INf(bp,e2,NONf(e3,r))), e4, f as INfix(lbp,rbp),err)=
	    if lbp > bp then INf(rbp,e4,p)
	     else (if lbp = bp
			then err WARN "mixed left- and right-associative \
				      \operators of same precedence"
			         nullErrorBody
			else ();
	           parse(NONf(app err(e2,pair err (e3,e1)),r),e4,f,err))
       | parse(p as NONf _, e',INfix(lbp,rbp),_) = INf(rbp,e',p)
     
     (* clean up the stack *)
     fun finish (NONf(e1,INf(_,e2,NONf(e3,r))),err) = 
		     finish(NONf(app err(e2,pair err (e3,e1)),r),err)
       | finish (NONf(e1,NILf),_) = e1
       | finish (INf(_,e1,NONf(e2,p)),err) = 
		     (err COMPLAIN "nonfix identifier required" nullErrorBody;
		      finish(NONf(app err(e2,e1),p),err))
       | finish (NILf,err) = impossible "Corelang.finish NILf"
       | finish _ = ErrorMsg.impossible "Corelang.finish"

  in {start=start,parse=parse,finish=finish}
 end
end

fun lookFIX(env,name) = (
  case Env.look(env,name) 
  of FIXbind(FIXvar{binding,...}) => binding
   | _ => impossible "lookFIX")
  handle Env.Unbound => NONfix

fun checkFix(int,err) =
    if 0 <= int andalso int <= 9
        then int
        else (err COMPLAIN "fixity precedence must be between 0 and 9"
	          nullErrorBody;
	      9)

(* Parsing expressions *)
val {start=exp_start, parse=exp_parse, finish=exp_finish} = 
	precedence(fn _ => fn(f,a) => AppExp{function=f,argument=a},
		   fn _ => fn (a,b) => TupleExp[a,b])

fun apply_pat err (VarPat d ,p) = AppPat{constr=d, argument=p}
  | apply_pat err _ = 
	(err COMPLAIN "non-constructor applied to argument in pattern"
	     nullErrorBody;
	 WildPat)

val {start=pat_start, parse=pat_parse0, finish=pat_finish} =
	precedence(apply_pat, 
		   fn err => fn (ap1,ap2) => TuplePat[ap1,ap2])

fun pat_parse(ap,(p,f,err)) = pat_parse0(ap, p, f,err)

(* verifies unicity of elements in a list *)
fun checkUniq (err,message) l =
 let val l' = Sort.sort Symbol.symbolGt l
     fun f (x::y::rest) = (if Symbol.eq(x,y) 
			      then err COMPLAIN(message^ ": " ^ Symbol.name x)
				       nullErrorBody
			      else ();
			   f(y::rest))
      | f _ = ()
  in f l' end

(* layered patterns *)
fun layered((x as VarPat _), y, _) = LayeredPat{varPat=x,expPat=y}
  | layered(ConstraintPat{pattern=x as VarPat _,constraint=t}, y, _) = 
	LayeredPat{varPat=x,expPat=ConstraintPat{pattern=y,constraint=t}}
  | layered(x,y,err) = (err COMPLAIN "pattern to left of AS must be variable"
			    nullErrorBody;
			y)

(* sequence of declarations *)
fun makeSEQdec (d1,d2) env = 
  let val (d1',f1) = d1 env
      val (d2',f2) = d2 (f1 env)
      val d' = 
        case (d1',d2')
        of (SeqDec a, SeqDec b) => SeqDec(a@b)
         | (SeqDec a, b) => SeqDec(a@[b])
         | (a, SeqDec b) => SeqDec(a::b)
         | (a,b) => SeqDec[a,b]
  in (d',f2 o f1) end

(* local declarations *)
fun makeLOCALdec (ldecs1,ldecs2) env =
  let val (ld1,f1) = ldecs1 env
      val (ld2,f2) = ldecs2 (f1 env)
  in (LocalDec(ld1,ld2),f2) end

(* let expressions *)
fun makeLETstr (ldec,str) env =
  let val (ld,f) = ldec env
      val str = str (f env)
  in LetStr (ld,str) end

fun makeLETfct (ldec,fct) env constraint =
  let val (ld,f) = ldec env
      val fct = fct((f env), constraint)
  in LetFct (ld,fct) end

(* val rec declarations *)
type rawrvb = {name:symbol,ty:ty option, match:parseEnv -> rule list }

fun makeVALRECdec (rvb,err) env =
    let val rvbs = rvb env
	fun makervb({name,ty,match,...}:rawrvb) =
	      Rvb{var=name,resultty=ty, exp=FnExp(match env)}
    in (ValrecDec(map makervb rvbs),Env.empty) end

type rawclause = {name:symbol,pats:pat list,resultty:ty option,
		  exp:parseEnv -> exp, err: ErrorMsg.complainer}

(* verification of function declarations *)

fun checkFB(clauses as ({name,pats,...}:rawclause)::rest, err) = (
      if exists (fn {name=n,...} => not(Symbol.eq(n,name))) rest
      then err COMPLAIN "clauses don't all have same function-name"
               nullErrorBody
      else ();clauses)
  | checkFB _ = ErrorMsg.impossible "CoreLang.checkFB"

fun make_app_pat((p as (_,_,err))::rest) =
    let fun f(x,p::r) = f(pat_parse(x,p),r)
	  | f(x,nil) = pat_finish(x,err)
     in f(pat_start p, rest)
    end
  | make_app_pat _ = ErrorMsg.impossible "make_app_pat"

fun checkpat(p,NONfix,_) = p
  | checkpat(p,INfix _, err) =
      (err COMPLAIN "NONfix pattern required" nullErrorBody;
       p)

fun funsym(VarPat [id], err) = id
  | funsym(_,err) = 
      (err COMPLAIN "illegal function symbol in clause" nullErrorBody;
       bogusID)

fun makecl(pats as _::_, [(a,INfix _,e), pat]) =
		(funsym(a,e), [TuplePat[make_app_pat pats, checkpat pat]])
  | makecl([(a,NONfix,_),(b,INfix _,e),(c,NONfix,_)],pats) =
		(funsym(b,e), TuplePat[a,c] :: map checkpat pats)
  | makecl([],[(a,NONfix,_),(b,INfix _,e),(c,NONfix,_)]) =
		(funsym(b,e), [TuplePat[a,c]])
  | makecl([],(a,NONfix,e)::(pats as _::_)) =
		(funsym(a,e), map checkpat pats)
  | makecl([],(a,INfix _,e)::(pats as _::_)) =
		(e COMPLAIN "INfix operator used without 'op' in fun dec"
		   nullErrorBody;
		 (funsym(a,e), map checkpat pats))
  | makecl(_,(_,_,e)::_) = (e COMPLAIN "can't find function symbol in fun dec"
			      nullErrorBody;
			    (bogusID,[WildPat]))
  | makecl((_,_,e)::_,_) = (e COMPLAIN "can't find function symbol in fun dec"
			      nullErrorBody;
			    (bogusID,[WildPat]))
  | makecl _ = ErrorMsg.impossible "CoreLang.makecl"

fun makeFUNdec (fb,err) env =
    let fun makevar (p as ({name,...}:rawclause)::_,l1,l2) = (name,p,l1,l2)
	  | makevar _ = ErrorMsg.impossible "makeFUNdec.makevar"
        val clauses = map makevar (fb env)
	fun makeclause{name,pats,resultty,exp,err} =
	   Clause{pats=pats,resultty=resultty,exp=exp env}
	fun evalclauses(v,l,l1,l2) = (v,map makeclause l,l1,l2)
	val fbs = map evalclauses clauses
	fun makefb (n,c,l1,l2) = MarkFb(Fb{var=n,clauses=c},l1,l2)
     in (FunDec(map makefb fbs),fn x => x) end

fun makeFIXdec(fixity,ops) _ =
  let fun bind (ident,env) =
	 Env.bind(ident,FIXbind(FIXvar{binding=fixity,name=ident}),env)
  in (FixDec {fixity=fixity,ops=ops},revfold bind ops) end

fun toplevelexp(env,exp) =
    (ValDec[Vb {exp = exp env, pat = VarPat itsym}],Env.empty)

fun QuoteExp s = AppExp{function=VarExp quoteDcon,argument=StringExp s}
fun AntiquoteExp e = AppExp{function=VarExp antiquoteDcon,argument= e}

end (* local *)
end (* structure *)

