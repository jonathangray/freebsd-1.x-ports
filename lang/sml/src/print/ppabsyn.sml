(* Copyright 1992 by AT&T Bell Laboratories *)
(* absyn/ppabsyn.sml *)

signature PPABSYN =
sig
  val ppPat    : Modules.env -> PrettyPrint.ppstream -> Absyn.pat * int -> unit
  val ppExp    : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.exp * int    -> unit
  val ppRule   : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.rule * int   -> unit
  val ppVB     : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.vb * int     -> unit
  val ppRVB    : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.rvb * int    -> unit
  val ppDec    : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.dec * int    -> unit
  val ppStrexp : Modules.env * Source.inputSource option -> PrettyPrint.ppstream
                 -> Absyn.strexp * int -> unit
end

structure PPAbsyn: PPABSYN = struct

open Absyn Access ErrorMsg Tuples
open Fixity Variables Types Modules
open PrettyPrint PPUtil
open PPType PPBasics

val lineprint = ref false

fun C f x y = f y x

fun prpos(ppstrm: PrettyPrint.ppstream,source: Source.inputSource,charpos: int) =
    if (!lineprint) then
      let val (file:string,line:int,pos:int) = Source.filepos source charpos
       in add_string ppstrm (makestring line);
	  add_string ppstrm ".";
	  add_string ppstrm (makestring pos)
      end
    else add_string ppstrm (makestring charpos)


fun nl_indent ppstrm i = add_break ppstrm (127,i)
    (* !!! 127 should be !line_width+1 *)

fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) = 
    Symbol.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun checkexp (n,nil) = true
  | checkexp (n, (LABEL{name=sym,...},_)::fields) = 
	Symbol.eq(sym, numlabel n) andalso checkexp(n+1,fields)

fun isTUPLEpat (RECORDpat{fields=[_],...}) = false
  | isTUPLEpat (RECORDpat{flex=false,fields,...}) = checkpat(1,fields)
  | isTUPLEpat _ = false
	
fun isTUPLEexp (RECORDexp [_]) = false
  | isTUPLEexp (RECORDexp fields) = checkexp(1,fields)
  | isTUPLEexp (MARKexp(a,_,_)) = isTUPLEexp a
  | isTUPLEexp _ = false

fun lookFIX (env,sym) =
    ModuleUtil.lookFIX (env,Symbol.fixSymbol(Symbol.name sym))

fun prunemark (MARKexp(a,_,_)) = prunemark a
  | prunemark x = x

fun ppvlist ppstrm (header,separator,pr_item,items) =
    case items
      of nil => ()
       | first::rest =>
	   (add_string ppstrm header;
	    pr_item ppstrm first;
	    app (fn x => (add_newline ppstrm;
			  add_string ppstrm separator;
			  pr_item ppstrm x))
		 rest)

fun ppPat env ppstrm =
    let val ppsay = add_string ppstrm
	fun ppPat' (_,0) = ppsay "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = ppsay "_"
	  | ppPat' (INTpat i,_) = ppsay (makestring i)
	  | ppPat' (REALpat r,_) = ppsay r
	  | ppPat' (STRINGpat s,_) = pp_mlstr ppstrm s
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppPat'(v,d); ppsay " as "; ppPat'(p,d-1);
	       end_block ppstrm)
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[],flex,...},_) =
	      if flex then ppsay "{...}"
	      else ppsay "()"
	  | ppPat' (r as RECORDpat{fields,flex,...},d) =
	      if isTUPLEpat r
	      then ppClosedSequence ppstrm
		     {front=(C add_string "("),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string ")"),
		      pr=(fn _ => fn (sym,pat) => ppPat'(pat,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C add_string "{"),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(fn ppstrm => if flex then add_string ppstrm ",...}"
				         else add_string ppstrm "}"),
		      pr=(fn ppstrm => fn (sym,pat) =>
			  (ppSym ppstrm sym; add_string ppstrm "=";
			   ppPat'(pat,d-1))),
		      style=INCONSISTENT}
		     fields
	  | ppPat' (VECTORpat(nil,_), d) = ppsay "#[]"
	  | ppPat' (VECTORpat(pats,_), d) = 
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in ppClosedSequence ppstrm
		    {front=(C add_string "#["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (0,0))),
		     back=(C add_string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    pats
	      end
	  | ppPat' (CONpat(e,_),_) = ppDcon ppstrm e
	  | ppPat' (p as APPpat _, d) =
	      let val noparen = INfix(0,0)
	       in ppDconPat (env,ppstrm) (p,noparen,noparen,d)
	      end
	  | ppPat' (CONSTRAINTpat (p,t),d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppPat'(p,d-1); ppsay " :";
	      add_break ppstrm (1,2);
	      ppType env ppstrm t;
	      end_block ppstrm)
     in ppPat'
    end

and ppDconPat(env,ppstrm) = 
    let val ppsay = add_string ppstrm
	fun ppDconPat'(_,_,_,0) = ppsay "<pat>"
	  | ppDconPat'(CONpat(DATACON{name,...},_),l:fixity,r:fixity,_) =
	      ppSym ppstrm name
	  | ppDconPat'(CONSTRAINTpat(p,t),l,r,d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppsay "("; ppPat env ppstrm (p,d-1); ppsay " :";
	      add_break ppstrm (1,2);
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm)
	  | ppDconPat'(LAYEREDpat(v,p),l,r,d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppsay "("; ppPat env ppstrm (v,d); add_break ppstrm (1,2);
	      ppsay " as "; ppPat env ppstrm (p,d-1); ppsay ")";
	      end_block ppstrm)
	  | ppDconPat'(APPpat(DATACON{name,...},_,p),l,r,d) =
	      let val dname = Symbol.name name
		  val fixity = lookFIX(env,name)
		  fun prdcon() =
		      case (fixity,isTUPLEpat p,p)
			of (INfix _,true,RECORDpat{fields=[(_,pl),(_,pr)],...}) =>
				   (ppDconPat'(pl,NONfix,fixity,d-1);
				    ppsay " "; ppsay dname; ppsay " ";
				    ppDconPat'(pr,fixity,NONfix,d-1))
			  | _ => (ppsay dname; ppsay " ";
				  ppDconPat'(p,NONfix,NONfix,d-1))
	      in  case (l,r,fixity)
		    of (NONfix,NONfix,_) => (ppsay "("; prdcon(); ppsay ")")
		     | (INfix _,INfix _,_) => prdcon()
		     | (_,_,NONfix) => prdcon()
		     | (INfix(_,p1),_,INfix(p2,_)) =>
			 if p1 >= p2
			 then (ppsay "("; prdcon(); ppsay ")")
			 else prdcon()
		     | (_,INfix(p1,_),INfix(_,p2)) =>
			 if p1 > p2
			 then (ppsay "("; prdcon(); ppsay ")")
			 else prdcon()
	      end
	  | ppDconPat' (p,_,_,d) = ppPat env ppstrm (p,d)
     in ppDconPat'
    end

fun trim [x] = nil | trim (a::b) = a::trim b

fun ppExp (context as (env,source_opt)) ppstrm =
    let val ppsay = add_string ppstrm
	fun ppExp'(_,0) = ppsay "<exp>"
	  | ppExp'(VARexp(ref var,_),_) = ppVar ppstrm var
	  | ppExp'(CONexp(con,_),_) = ppDcon ppstrm con
	  | ppExp'(INTexp i,_) = ppsay(makestring i)
	  | ppExp'(REALexp r,_) = ppsay r
	  | ppExp'(STRINGexp s,_) = pp_mlstr ppstrm s
	  | ppExp'(r as RECORDexp fields,d) =
	      if isTUPLEexp r
	      then ppClosedSequence ppstrm
		     {front=(C add_string "("),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string ")"),
		      pr=(fn _ => fn (_,exp) => ppExp'(exp,d-1)),
		      style=INCONSISTENT}
		     fields
	      else ppClosedSequence ppstrm
		     {front=(C add_string "{"),
		      sep=(fn ppstrm => (add_string ppstrm ",";
					 add_break ppstrm (0,0))),
		      back=(C add_string "}"),
		      pr=(fn ppstrm => fn (LABEL{name,...},exp) =>
			  (ppSym ppstrm name; ppsay "=";
			   ppExp'(exp,d))),
		      style=INCONSISTENT}
		     fields
	  | ppExp'(VECTORexp nil, d) = ppsay "#[]"
	  | ppExp'(VECTORexp exps, d) =
	      let fun pr _ exp = ppExp'(exp,d-1)
	      in  ppClosedSequence ppstrm
		    {front=(C add_string "#["),
		     sep=(fn ppstrm => (add_string ppstrm ",";
					add_break ppstrm (1,0))),
		     back=(C add_string "]"),
		     pr=pr,
		     style=INCONSISTENT}
		    exps
	      end
	  | ppExp'(SEQexp exps,d) =
	      ppClosedSequence ppstrm
	        {front=(C add_string "("),
		 sep=(fn ppstrm => (add_string ppstrm ";";
				    add_break ppstrm (1,0))),
		 back=(C add_string ")"),
		 pr=(fn _ => fn exp => ppExp'(exp,d-1)),
		 style=INCONSISTENT}
		exps
	  | ppExp'(e as APPexp _,d) =
	      let val noparen = INfix(0,0)
	      in  ppAppExp (e,noparen,noparen,d)
	      end
	  | ppExp'(CONSTRAINTexp(e, t),d) =
	     (begin_block ppstrm INCONSISTENT 0;
	      ppsay "("; ppExp'(e,d); ppsay ":";
	      add_break ppstrm (1,2);
	      ppType env ppstrm t; ppsay ")";
	      end_block ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER(FNexp(rules,_))),d) =
	     (begin_block ppstrm CONSISTENT 0;
	      ppExp'(exp,d-1); add_newline ppstrm; ppsay "handle ";
	      nl_indent ppstrm 2;
	      ppvlist ppstrm ("  ","| ",
		  (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), rules);
	      end_block ppstrm)
	  | ppExp'(HANDLEexp(exp, HANDLER _),d) =
	      impossible "ppExp':impossible handler"
	  | ppExp'(RAISEexp(exp,_),d) = 
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "raise "; ppExp'(exp,d-1);
	       end_block ppstrm)
	  | ppExp'(LETexp(dec, exp),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec context ppstrm (dec,d-1); add_break ppstrm (1,0);
	       ppsay " in "; ppExp'(exp,d-1); add_break ppstrm (1,0);
	       ppsay "end";
	       end_block ppstrm)
	  | ppExp'(CASEexp(exp, rules),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "(case "; ppExp'(exp,d-1); nl_indent ppstrm 2;
	       ppvlist ppstrm ("of ","   | ",
		 (fn ppstrm => fn r => ppRule context ppstrm (r,d-1)), trim rules);
	       ppsay ")";
	       end_block ppstrm)
	  | ppExp'(FNexp(rules,_),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppvlist ppstrm ("(fn ","  | ",
			       (fn ppstrm => fn r =>
				  ppRule context ppstrm (r,d-1)),
			       trim rules);
	       ppsay ")";
	       end_block ppstrm)
	  | ppExp'(MARKexp (exp,s,e),d) =
	      (case source_opt
		of SOME source =>
	           (ppsay "MARKexp(";
		    ppExp'(exp,d); ppsay ",";
		    prpos(ppstrm,source,s); ppsay ",";
		    prpos(ppstrm,source,e); ppsay ")")
	         | NONE => ppExp'(exp,d))

	and ppAppExp (_,_,_,0) = add_string ppstrm "<exp>"
	  | ppAppExp arg =
	    let val ppsay = add_string ppstrm
		fun fixitypp(name,e,l,r,d) =
		    let val dname = formatQid name
			val fixity = case name of [id] => lookFIX(env,id)
						| _ => NONfix
			fun pr() =
			    case (fixity,isTUPLEexp e,prunemark e)
			      of (INfix _,true,RECORDexp[(_,pl),(_,pr)]) =>
				    (begin_block ppstrm INCONSISTENT 2;
				     ppAppExp (pl,NONfix,fixity,d-1);
				     add_break ppstrm (1,0); ppsay dname;
				     add_break ppstrm (1,0);
				     ppAppExp (pr,fixity,NONfix,d-1);
				     end_block ppstrm)
				| _ =>
				    (begin_block ppstrm INCONSISTENT 2;
				     ppsay dname; add_break ppstrm (1,0);
				     ppAppExp (e,NONfix,NONfix,d-1);
				     end_block ppstrm)
		    in  case (l,r,fixity)
			  of (NONfix,NONfix,_) => (ppsay "("; pr(); ppsay ")")
			   | (INfix _,INfix _,_) => pr()
			   | (_,_,NONfix) => pr()
			   | (INfix(_,p1),_,INfix(p2,_)) =>
				if p1 >= p2 then (ppsay "("; pr(); ppsay ")")
				else pr()
			   | (_,INfix(p1,_),INfix(_,p2)) =>
				if p1 > p2 then (ppsay "("; pr(); ppsay ")")
				else pr()
		    end
		fun appPrint(_,_,_,0) = ppsay "#"
		  | appPrint(CONSTRAINTexp(e,t),l,r,d) =
		      (begin_block ppstrm INCONSISTENT 2;
		       ppsay "("; ppExp'(e,d-1);
		       ppsay " :"; add_break ppstrm (1,1);
		       ppType env ppstrm t; ppsay ")";
		       end_block ppstrm)
		  | appPrint(APPexp(CONexp(DATACON{name,...},_),e),l,r,d) =
		      fixitypp([name],e,l,r,d)
		  | appPrint(APPexp(VARexp(ref(VALvar{name,...}),_),e),l,r,d) =
		      fixitypp(name,e,l,r,d)
		  | appPrint(APPexp(VARexp(ref(OVLDvar{name,...}),_),e),l,r,d) =
		      fixitypp([name],e,l,r,d)
		  | appPrint(APPexp(app as APPexp _,rand),NONfix,NONfix,d) =
		      let val yesparen = INfix(0,100000000) (* a hack *)
		       in begin_block ppstrm INCONSISTENT 2;
			  ppsay "("; appPrint(app,yesparen,NONfix,d-1);
			  add_break ppstrm (1,2);
			  appPrint(rand,NONfix,NONfix,d-1); ppsay ")";
			  end_block ppstrm
		      end
		  | appPrint(APPexp(app as APPexp _,rand),l,r,d) =
		      let val yesparen = INfix(0,100000000) (* a hack *)
		       in begin_block ppstrm INCONSISTENT 2;
			  appPrint(app,yesparen,NONfix,d-1);
			  add_break ppstrm (1,2);
			  appPrint(rand,NONfix,NONfix,d-1);
			  end_block ppstrm
		      end
		  | appPrint(APPexp(rator,rand),_,_,d) =
		      (begin_block ppstrm INCONSISTENT 2;
		       ppExp'(rator,d-1);
		       add_break ppstrm (1,2); ppExp'(rand,d-1);
		       end_block ppstrm)
		  | appPrint(MARKexp(exp,s,e),l,r,d) =
		      (case source_opt
			of SOME source =>
			   (ppsay "MARKexp(";
			    appPrint(exp,l,r,d); ppsay ",";
			    prpos(ppstrm,source,s); ppsay ",";
			    prpos(ppstrm,source,e); ppsay ")")
			 | NONE => appPrint(exp,l,r,d))
		  | appPrint (e,_,_,d) = ppExp'(e,d)
	     in appPrint arg
	    end
     in ppExp'
    end

and ppRule (context as (env,source_opt)) ppstrm (RULE(pat,exp),d) =
    if d>0
    then (begin_block ppstrm CONSISTENT 0;
	  ppPat env ppstrm (pat,d-1);
	  add_string ppstrm " =>"; add_break ppstrm (1,0);
	  ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<rule>"

and ppVB (context as (env,source_opt)) ppstrm (VB{pat,exp,...},d) =
    if d>0
    then (begin_block ppstrm CONSISTENT 0;
	  ppPat env ppstrm (pat,d-1); add_string ppstrm " =";
	  add_break ppstrm (1,2); ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<binding>"

and ppRVB context ppstrm (RVB{var,exp,...},d) = 
    if d>0
    then (begin_block ppstrm INCONSISTENT 0;
	  ppVar ppstrm var; add_string ppstrm " =";
	  add_break ppstrm (1,2); ppExp context ppstrm (exp,d-1);
	  end_block ppstrm)
    else add_string ppstrm "<rec binding>"

and ppDec (context as (env,source_opt)) ppstrm =
  let val ppsay = add_string ppstrm
      fun ppDec'(_,0) = ppsay "<dec>"
      | ppDec'(VALdec vbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("val ","and ",
	     (fn ppstrm => fn vb => ppVB context ppstrm (vb,d-1)),vbs);
	   end_block ppstrm)
      | ppDec'(VALRECdec rvbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("val rec ","and ",
	     (fn ppstrm => fn rvb => ppRVB context ppstrm (rvb,d-1)),rvbs);
	   end_block ppstrm)
      | ppDec'(TYPEdec tbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("type "," and ",
	    (fn ppstrm =>
	     (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...},...},def}) =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); ppsay " ");
		  ppSym ppstrm name; ppsay " = "; ppType env ppstrm def)
	       | _ => impossible "ppabsyn.398")),
	     tbs);
	   end_block ppstrm)
      | ppDec'(DATATYPEdec{datatycs,withtycs},d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("datatype ","and ",
	    (fn ppstrm =>
	     (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); ppsay " ");
		  ppSym ppstrm name; ppsay " = ";
		  ppSequence ppstrm
		    {sep=(fn ppstrm => (add_string ppstrm " |";
					add_break ppstrm (1,0))),
		     pr=(fn ppstrm => fn (DATACON{name,...}) => ppSym ppstrm name),
		     style=INCONSISTENT}
		    dcons)
	       | _ => impossible "ppabsyn.8")),
	     datatycs);
	   add_newline ppstrm;
	   ppvlist ppstrm ("withtype ","and ",
	    (fn ppstrm =>
	     (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...},...},def}) =>
		 (case arity
		    of 0 => ()
		     | 1 => (ppsay "'a ")
		     | n => (ppTuple ppstrm add_string (typeFormals n); ppsay " ");
		  ppSym ppstrm name; ppsay " = "; ppType env ppstrm def)
	       | _ => impossible "ppabsyn.398")),
	     withtycs);
	   end_block ppstrm)
      | ppDec'(ABSTYPEdec _,d) = ppsay "abstype"
      | ppDec'(EXCEPTIONdec ebs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("exception ","and ",
	    (fn ppstrm =>
	     (fn (EBgen{exn=DATACON{name,...},etype,...}) =>
		   (ppSym ppstrm name;
		    case etype
		      of NONE => ()
		       | SOME ty' =>
			  (ppsay " of "; ppType env ppstrm ty'))
	       | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
		   (ppSym ppstrm name; ppsay "="; ppSym ppstrm dname))),
	     ebs);
	   end_block ppstrm)
      | ppDec'(STRdec sbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("structure ","and ",
	    (fn ppstrm =>
	     (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
		 (ppSym ppstrm name; ppAccess ppstrm access; ppsay " = ";
		  add_break ppstrm (1,2); ppStrexp context ppstrm (def,d-1)))),
	     sbs);
	   end_block ppstrm)
      | ppDec'(ABSdec sbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("abstraction ","and ",
	    (fn ppstrm =>
	     (fn (STRB{strvar=STRvar{access,name,...},def,...}) =>
		 (ppSym ppstrm name; ppAccess ppstrm access; ppsay " = ";
		  add_break ppstrm (1,2); ppStrexp context ppstrm (def,d-1)))),
	     sbs);
	   end_block ppstrm)
      | ppDec'(FCTdec fbs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppvlist ppstrm ("functor ","and ",
	    (fn ppstrm =>
	     (fn (FCTB{fctvar=FCTvar{access,name=fname,...},
		       def=FCTfct{param=STRvar{name=pname,...},def,...}}) =>
		   (ppSym ppstrm fname; ppAccess ppstrm access; ppsay " ("; 
		    ppSym ppstrm pname; ppsay ") = "; add_newline ppstrm;
		    ppStrexp context ppstrm (def,d-1))
               | (FCTB{fctvar=FCTvar{access,name=fname,...},
		       def=VARfct{def=FCTvar{name=fname',...},...}}) =>
		   (ppSym ppstrm fname; ppAccess ppstrm access; ppsay " = "; 
		    ppSym ppstrm fname'))),
	     fbs);
	   end_block ppstrm)
      | ppDec'(SIGdec sigvars,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn SIGvar{name,...} =>
		    (ppsay "signature "; ppSym ppstrm name)),
	      style=CONSISTENT}
	     sigvars;
	   end_block ppstrm)
      | ppDec'(FSIGdec sigvars,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn FSIGvar{name,...} =>
		    (ppsay "funsig "; ppSym ppstrm name)),
	      style=CONSISTENT}
	     sigvars;
	   end_block ppstrm)
      | ppDec'(LOCALdec(inner,outer),d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppsay "local"; nl_indent ppstrm 2;
	   ppDec'(inner,d-1); add_newline ppstrm;
	   ppsay "in ";
	   ppDec'(outer,d-1); add_newline ppstrm;
	   ppsay "end";
	   end_block ppstrm)
      | ppDec'(SEQdec decs,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppSequence ppstrm
	     {sep=add_newline,
	      pr=(fn ppstrm => fn dec => ppDec'(dec,d)),
	      style=CONSISTENT}
	     decs;
	   end_block ppstrm)
      | ppDec'(FIXdec {fixity,ops},d) =
	  (begin_block ppstrm CONSISTENT 0;
	   case fixity
	     of NONfix => ppsay "nonfix "
	      | INfix (i,_) => 
		    (if i mod 2 = 0 then 
		       ppsay "infix "
		     else ppsay "infixr ";
		     if i div 2 > 0 then
		       (ppsay(makestring(i div 2));
			ppsay " ")
		     else ());
	   ppSequence ppstrm
	     {sep=(fn ppstrm => add_break ppstrm (1,0)),
	      pr=ppSym,style=INCONSISTENT}
	     ops;
	   end_block ppstrm)
      | ppDec'(OVLDdec ovldvar,d) =
	  (ppsay "overload "; ppVar ppstrm ovldvar)
      | ppDec'(OPENdec strVars,d) =
	  (begin_block ppstrm CONSISTENT 0;
	   ppsay "open ";
	   ppSequence ppstrm
	     {sep=(fn ppstrm => add_break ppstrm (1,0)),
	      pr=(fn ppstrm => fn STRvar{name,...} => ppSym ppstrm name),
	      style=INCONSISTENT}
	     strVars;
	   end_block ppstrm)
      | ppDec'(MARKdec(dec,s,e),d) = 
	  (case source_opt
	    of SOME source =>
	       (ppsay "MARKdec(";
		ppDec'(dec,d); ppsay ",";
		prpos(ppstrm,source,s); ppsay ",";
		prpos(ppstrm,source,e); ppsay ")")
	     | NONE => ppDec'(dec,d))
   in ppDec'
  end

and ppStrexp (context as (_,source_opt)) ppstrm =
    let val ppsay = add_string ppstrm
	fun ppStrexp'(_,0) = ppsay "<strexp>"
	  | ppStrexp'(VARstr(STRvar{access,name,...}),d) = ppSym ppstrm name
	  | ppStrexp'(STRUCTstr{body,...},d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "struct"; nl_indent ppstrm 2;
	       ppSequence ppstrm
		 {sep=add_newline,
		  pr=(fn ppstrm => fn dec => ppDec context ppstrm (dec,d-1)),
		  style=CONSISTENT}
		 body;
	       ppsay "end";
	       end_block ppstrm)
	  | ppStrexp'(APPstr{oper=FCTvar{name,...}, argexp,...},d) =
	      (ppSym ppstrm name; ppsay"(";
	       ppStrexp'(argexp,d-1);
	       ppsay")")
	  | ppStrexp'(LETstr(dec,body),d) =
	      (begin_block ppstrm CONSISTENT 0;
	       ppsay "let "; ppDec context ppstrm (dec,d-1); add_newline ppstrm;
	       ppsay " in "; ppStrexp'(body,d-1); add_newline ppstrm;
	       ppsay "end";
	       end_block ppstrm)
	  | ppStrexp'(MARKstr(body,s,e),d) =
	      (case source_opt
		of SOME source =>
	           (ppsay "MARKstr(";
		    ppStrexp'(body,d); ppsay ",";
		    prpos(ppstrm,source,s); ppsay ",";
		    prpos(ppstrm,source,e); ppsay ")")
	         | NONE => ppStrexp'(body,d))
     in ppStrexp'
    end

end (* structure PPAbsyn *)
