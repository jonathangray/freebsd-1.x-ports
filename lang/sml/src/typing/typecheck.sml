(* Copyright 1989 by AT&T Bell Laboratories *)

structure Typecheck : TYPECHECK =
struct

open Array List Types Variables Access BasicTypes TypesUtil Unify Absyn
     Overload ErrorMsg PrettyPrint PPUtil PPType PPAbsyn
infix 9 sub
infix -->

val printDepth = System.Print.printDepth

fun refNewDcon(DATACON{name,const,rep,typ,sign}) = 
       DATACON{name=name,const=const,rep=rep,typ=refPatType,sign=sign}

val sortFields = Sort.sort (fn ((LABEL{number=n1,...},_),
				(LABEL{number=n2,...},_)) => n1>n2)
fun map2 f nil = (nil,nil)
  | map2 f (hd::tl) = let val (x,y) = f(hd)
                          val (xl,yl) = map2 f tl
		       in (x::xl,y::yl)
                      end

exception NotThere

fun decType(env,dec,toplev,err,loc) = 

let
val ppType = PPType.ppType env
val ppPat = PPAbsyn.ppPat env
val ppExp = PPAbsyn.ppExp(env,NONE)
val ppRule = PPAbsyn.ppRule(env,NONE)
val ppVB = PPAbsyn.ppVB(env,NONE)
val ppRVB = PPAbsyn.ppRVB(env,NONE)

fun generalizeTy(VALvar{typ,name=[n],...}, userbound: tyvar list,
		 occ:occ, loc) : unit =
    let val should_complain = ref true
	fun complain() = 
	      if !should_complain  (* don't complain again *)
	      then (should_complain := false;
		    err loc COMPLAIN "nongeneric weak type variable"
		     (fn ppstrm =>
		        (add_newline ppstrm; ppSym ppstrm n;
			 add_string ppstrm " : "; ppType ppstrm (!typ))))
	      else ()
	val index = ref 0  (* counts no of type variables bound *)
	fun next() = !index before inc index
	val sign = ref([]: {weakness:int,eq:bool} list)
	val uenv = array(length userbound, UNDEFty)
	fun pos(tv,tv'::rest) = if eqTyvar(tv,tv') then 0 else pos(tv,rest)+1
	  | pos(_,[]) = raise NotThere
	val menv = ref([]: (tyvar*ty) list)
	fun lookup tv =
	    let fun find [] = raise NotThere
		  | find((tv',ty)::rest) = if eqTyvar(tv,tv') then ty 
							      else find rest
	     in find(!menv)
	    end
	fun bind(b as (_,ty)) = (menv := b::(!menv); ty)
	fun gen(ty) =     
	    case ty
	     of VARty(ref(INSTANTIATED ty)) => gen ty
	      | VARty(tv as ref(OPEN{depth,weakness,eq,kind})) =>
		  (case kind
		     of FLEX _ =>
			 (err loc COMPLAIN "unresolved flex record in let pattern"
			    (fn ppstrm =>
			       (PPType.resetPPType();
				add_newline ppstrm;
				add_string ppstrm "type: ";
				ppType ppstrm ty));
			  WILDCARDty)
		      | META =>
			  if depth > lamdepth occ
			  then if weakness > generalize_point occ
			       then lookup tv
				 handle NotThere =>
				 (sign := {weakness=weakness,eq=eq} :: !sign;
				  bind(tv,IBOUND (next()) ))
			       else (if toplevel occ then complain() else ();
				     ty)
			  else ty (* raise SHARE *)
		      | UBOUND name =>
		 (let val i = pos(tv,userbound)
		   in if depth > lamdepth occ
		      then case (uenv sub i)
			    of UNDEFty =>
			       let val weakness = 
				     if weakness > generalize_point occ
				     then weakness
				     else (complain(); generalize_point occ+1)
				   val new = IBOUND(next())
				in update(uenv,i,new);
				   sign := {weakness=weakness,eq=eq}
					    :: !sign;
				   new
			       end
			     | ty => ty  (* raise SHARE *)
		      else (err loc COMPLAIN
			     ("explicit type variable cannot be generalized \
			       \at scoping declaration: " ^
			      (tyvar_printname tv))
			     nullErrorBody;
			    tv := INSTANTIATED WILDCARDty;
			    WILDCARDty)
		  end
		  handle NotThere => ty))
	      | CONty(tyc,args) => CONty(tyc, map gen args) (*shareMap*)
	      | WILDCARDty => WILDCARDty
	      | _ => (err loc COMPLAIN "generalizeTy -- bad arg" nullErrorBody;
		      (* conjecture that this error is impossible now *)
		      WILDCARDty)
	val ty = gen(!typ)
     in typ := POLYty{sign = rev(!sign), abs = abscount occ,
		      tyfun = TYFUN{arity=(!index),body=ty}}
	(* always produce a POLYty, even when no variables are generalized.
	   this is to save the abs value for use in instantiateType below. *)
    end
  | generalizeTy _ = impossible "typecheck generlizeTy 121"
  
fun generalizePat(pat: pat, userbound: tyvar list, occ: occ, loc) =
    let val rec gen = fn VARpat v => generalizeTy(v,userbound,occ,loc)
	       	       | RECORDpat{fields,...} => app (gen o #2) fields
	               | APPpat(_,_,arg) => gen arg
	               | CONSTRAINTpat(pat,_) => gen pat
	               | LAYEREDpat(varPat,pat) => (gen varPat; gen pat)
	               | _ => ()
     in gen pat
    end

fun applyType(ratorTy: ty, randTy: ty) : ty =
    let val resultType = mkMETAty()
     in unifyTy(ratorTy, (randTy --> resultType));
	resultType
    end

fun patType(pat: pat, depth, loc) : pat * ty =
    case pat
      of WILDpat => (pat,mkRefMETAty depth)
       | VARpat(VALvar{typ as ref UNDEFty,...}) => 
	      (typ := mkRefMETAty depth; (pat,!typ))
       | INTpat _ => (pat,intTy)
       | REALpat _ => (pat,realTy)
       | STRINGpat _ => (pat,stringTy)
       | CONpat(dcon as DATACON{typ,...},_) => 
           let val ty = applyPoly(typ,Root)
            in case typ of POLYty _ => (CONpat(dcon,SOME ty),ty)
                         | _ => (CONpat(dcon,NONE),ty)
           end
       | RECORDpat{fields,flex,typ,pats} =>
	   (* fields assumed already sorted by label *)
	   let fun g(lab,pat') = 
                 let val (npat,nty) = patType(pat',depth,loc)
                  in ((lab,npat), (lab,nty))
                 end
               val (fields',labtys) = map2 g fields
               val npat = RECORDpat{fields=fields',flex=flex,typ=typ,pats=pats}
	    in if flex
	       then let val ty = VARty(mkTyvar(mkFLEX(labtys,depth)))
		     in typ := ty; (npat,ty)
		    end
	       else (npat,recordTy(labtys))
	   end
       | VECTORpat(pats,_) => 
          (let val (npats,ntys) = map2 (fn pat => patType(pat,depth,loc)) pats
               val nty = fold (fn (a,b) => (unifyTy(a,b); b))
                            ntys (mkRefMETAty depth)
            in (VECTORpat(npats,nty), CONty(vectorTycon,[nty]))
           end  handle Unify(mode) => (
		err loc COMPLAIN("vector pattern type failure") nullErrorBody;
		(pat,WILDCARDty)))
       | APPpat(dcon as DATACON{typ,rep,...},_,arg) =>
	   let val (argpat,argty) = patType(arg,depth,loc)
               val (ty1,ndcon) = case rep 
                         of Access.REF => (refPatType,refNewDcon dcon)
                          | _ => (typ,dcon)
               val ty2 = applyPoly(ty1,Root)
               val npat = case typ of POLYty _ => APPpat(ndcon,SOME ty2,argpat)
                                    | _ => APPpat(ndcon,NONE,argpat)
            in (npat,applyType(ty2,argty))
	       handle Unify(mode) =>
		(err loc COMPLAIN
                  ("constructor and argument don't agree in pattern ("^mode^")")
		  (fn ppstrm =>
		   (PPType.resetPPType();
		    add_newline ppstrm;
		    add_string ppstrm "constructor: ";
		    ppType ppstrm typ; add_newline ppstrm;
		    add_string ppstrm "argument:    ";
		    ppType ppstrm argty; add_newline ppstrm;
		    add_string ppstrm "in pattern:"; add_break ppstrm (1,2);
		    ppPat ppstrm (pat,!printDepth)));
		 (pat,WILDCARDty))
	   end
       | CONSTRAINTpat(pat',ty) => 
	   let val (npat,patTy) = patType(pat',depth,loc)
	    in (unifyTy(patTy, ty); (CONSTRAINTpat(npat,ty),ty))
	       handle Unify(mode) =>
	         (err loc COMPLAIN
		    ("pattern and constraint don't agree (" ^ mode ^ ")")
		    (fn ppstrm =>
		     (PPType.resetPPType();
		      add_newline ppstrm;
		      add_string ppstrm "pattern:    ";
		      ppType ppstrm patTy; add_newline ppstrm;
		      add_string ppstrm "constraint: ";
		      ppType ppstrm ty; add_newline ppstrm;
		      add_string ppstrm "in pattern:"; add_break ppstrm (1,2);
		      ppPat ppstrm (pat,!printDepth)));
	          (pat,WILDCARDty))
	   end
       | LAYEREDpat(vpat as VARpat(VALvar{typ,...}),pat') =>
           let val (npat,patTy) = patType(pat',depth,loc)
               val _ = (typ := patTy)
            in (LAYEREDpat(vpat,npat),patTy)
           end
       | LAYEREDpat(cpat as CONSTRAINTpat(VARpat(VALvar{typ,...}),ty),pat') =>
	   let val (npat,patTy) = patType(pat',depth,loc)
	    in (unifyTy(patTy,ty); typ := ty; 
                (LAYEREDpat(cpat,npat),ty))
	       handle Unify(mode) =>
	         (err loc COMPLAIN
		    ("pattern and constraint don't agree (" ^ mode ^ ")")
		    (fn ppstrm =>
		     (PPType.resetPPType();
		      add_newline ppstrm;
		      add_string ppstrm "pattern:    ";
		      ppType ppstrm patTy; add_newline ppstrm;
		      add_string ppstrm "constraint: ";
		      ppType ppstrm ty; add_newline ppstrm;
		      add_string ppstrm "in pattern:"; add_break ppstrm (1,2);
		      ppPat ppstrm (pat,!printDepth)));
	          (pat,WILDCARDty))
	   end
       | p => impossible "patType -- unexpected pattern"

fun expType(exp: exp, occ: occ, loc) : exp * ty =
    case exp
      of VARexp(r as ref(VALvar{typ,access,name}),_) => 
	   let val ty = instantiateType(!typ,occ)
                    (* was: applyPoly(!typ,abscount occ,base occ,wmax occ) *)
	    in if Prim.special access  (*  =, <>, :=, update  special cases *)
	       then (r := VALvar{typ= ref ty,access=access,name=name};
                     (VARexp(r,NONE),ty))
	       else (case (!typ) 
                      of POLYty _ => (VARexp(r,SOME ty),ty)
                       | _ => (VARexp(r,NONE),ty))
	   end
       | VARexp(refvar as ref(OVLDvar _),_) =>
 	    (exp,pushOverloaded(refvar, err loc))
       | CONexp(dcon as DATACON{typ,...},_) => 
           let val ty = applyPoly(typ,occ)
            in case typ 
                of POLYty _ => (CONexp(dcon,SOME ty),ty)
                 | _ => (CONexp(dcon,NONE),ty)
           end
       | INTexp _ => (exp,intTy)
       | REALexp _ => (exp,realTy)
       | STRINGexp _ => (exp,stringTy)
       | RECORDexp fields =>
           let fun h(l as LABEL{name,...},exp') = 
                    let val (nexp,nty) = expType(exp',occ,loc)
                     in ((l,nexp),(l,nty))
                    end
               fun extract(LABEL{name,...},t) = (name,t)
               val (fields',tfields) = map2 h fields
               val rty = map extract (sortFields tfields)
            in (RECORDexp fields',recordTy(rty))
           end
       | VECTORexp exps =>
          (let val (exps',nty) = map2 (fn e => expType(e,occ,loc)) exps
               val vty = fold (fn (a,b) => (unifyTy(a,b); b)) nty (mkMETAty())
            in (VECTORexp exps', CONty(vectorTycon,[vty]))
           end handle Unify(mode) =>
		 (err loc COMPLAIN ("vector expression type failure (" ^mode^ ")")
		    nullErrorBody;
		  (exp,WILDCARDty)))
       | SEQexp exps => 
	   let fun scan nil = (nil,unitTy)
	         | scan [e] = 
                     let val (e',ety) = expType(e,occ,loc)
                      in ([e'],ety)
                     end
		 | scan (e::rest) = 
                     let val (e',_) = expType(e,occ,loc)
                         val (el',ety) = scan rest
                      in (e'::el',ety)
                     end
               val (exps',expty) = scan exps
            in (SEQexp exps',expty)
	   end
       | APPexp(rator, rand) =>
	   let val (rator',ratorTy) = expType(rator,Rator occ,loc)
	       val (rand',randTy) = expType(rand,Rand occ,loc)
               val exp' = APPexp(rator',rand')
	    in (exp',applyType(ratorTy,randTy))
	       handle Unify(mode) => 
	       let val ratorTy = prune ratorTy
		   val reducedRatorTy = headReduceType ratorTy
		in PPType.resetPPType();
		   if isArrowType(reducedRatorTy)
		   then (err loc COMPLAIN
			   ("operator and operand don't agree (" ^ mode ^ ")")
			   (fn ppstrm =>
			    (add_newline ppstrm;
			     add_string ppstrm "operator domain: ";
			     ppType ppstrm (domain reducedRatorTy);
			     add_newline ppstrm;
			     add_string ppstrm "operand:         ";
			     ppType ppstrm randTy; add_newline ppstrm;
			     add_string ppstrm "in expression:";
			     add_break ppstrm (1,2);
			     ppExp ppstrm (exp,!printDepth)));
			 (exp,WILDCARDty))
		   else (err loc COMPLAIN "operator is not a function"
			  (fn ppstrm =>
			    (add_newline ppstrm;
			     add_string ppstrm "operator: ";
			     ppType ppstrm (ratorTy); add_newline ppstrm;
			     add_string ppstrm "in expression:";
			     add_break ppstrm (1,2);
			     ppExp ppstrm (exp,!printDepth)));
			 (exp,WILDCARDty))
	       end
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val (e',ety) = expType(e,occ,loc)
	    in (unifyTy(ety, ty); (CONSTRAINTexp(e',ty),ty))
	       handle Unify(mode) =>
	         (err loc COMPLAIN
		    ("expression and constraint don't agree (" ^ mode ^ ")")
		    (fn ppstrm =>
		      (PPType.resetPPType();
		       add_newline ppstrm;
		       add_string ppstrm "expression: ";
		       ppType ppstrm ety; add_newline ppstrm;
		       add_string ppstrm "constraint: ";
		       ppType ppstrm ty; add_newline ppstrm;
		       add_string ppstrm "in expression:"; add_break ppstrm (1,2);
		       ppExp ppstrm (e,!printDepth)));
		  (exp,WILDCARDty))
	   end
       | HANDLEexp(e,HANDLER h) =>
	   let val (e',ety) = expType(e,occ,loc)
	       and (h',hty) = expType(h,occ,loc)
               val exp' = HANDLEexp(e',HANDLER h')
	    in (unifyTy(hty, exnTy --> ety); (exp',ety))
	       handle Unify(mode) =>
	         let val hty = prune hty
		  in PPType.resetPPType();
		     if ((unifyTy(domain hty,exnTy); false) handle Unify _ => true)
		     then (err loc COMPLAIN "handler domain is not exn"
			    (fn ppstrm =>
			     (add_newline ppstrm;
			      add_string ppstrm "handler domain: ";
			      ppType ppstrm (domain hty); add_newline ppstrm;
			      add_string ppstrm "in expression:";
			      add_break ppstrm (1,2);
			      ppExp ppstrm (exp,!printDepth))))
		     else (err loc COMPLAIN
			    ("expression and handler don't agree (" ^ mode ^ ")")
			    (fn ppstrm => 
			     (add_newline ppstrm;
			      add_string ppstrm "body:          ";
			      ppType ppstrm ety; add_newline ppstrm;
			      add_string ppstrm "handler range: ";
			      ppType ppstrm (range hty); add_newline ppstrm;
			      add_string ppstrm "in expression:";
			      add_break ppstrm (1,2);
			      ppExp ppstrm (exp,!printDepth))));
		     (exp,WILDCARDty)
		 end
	   end
       | RAISEexp(e,_) =>
	   let val (e',ety) = expType(e,occ,loc)
               val newty = mkMETAty()
	    in unifyTy(ety,exnTy)
	       handle Unify(mode) =>
		(err loc COMPLAIN "argument of raise is not an exception"
		  (fn ppstrm =>
		   (PPType.resetPPType();
		    add_newline ppstrm; add_string ppstrm "raised: ";
		    ppType ppstrm ety; add_newline ppstrm;
		    add_string ppstrm "in expression:"; add_break ppstrm (1,2);
		    ppExp ppstrm (exp,!printDepth))));
	       (RAISEexp(e',ety --> newty),newty)
	   end
       | LETexp(d,e) => 
           let val d' = decType0(d,LetDef(occ),loc)
               val (e',ety) = expType(e,occ,loc)
            in (LETexp(d',e'),ety)
           end
       | CASEexp(e,rules) =>
	   let val (e',ety) = expType(e,occ,loc)
	       and (rules',_,rty) = matchType(rules,Rator occ,loc)
               val exp' = CASEexp(e',rules')
	    in (exp',applyType(rty,ety))
	       handle Unify(mode) => 
	       (err loc COMPLAIN
		 ("case object and rules don't agree (" ^ mode ^ ")")
		 (fn ppstrm =>
	          (PPType.resetPPType();
		   add_newline ppstrm; add_string ppstrm "rule domain: ";
		   ppType ppstrm (domain rty); add_newline ppstrm;
		   add_string ppstrm "object:      ";
		   ppType ppstrm ety; add_newline ppstrm;
		   add_string ppstrm "in expression:"; add_break ppstrm (1,2);
		   ppExp ppstrm (exp,!printDepth)));
	        (exp,WILDCARDty))
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp(rules,_) => 
           let val (rules',ty,rty) = matchType(rules,occ,loc)
            in (FNexp(rules',ty),rty)
           end
       | MARKexp(e,locL,locR) => 
           let val (e',et) = expType(e,occ,(locL,locR))
            in (MARKexp(e',locL,locR),et)
           end
       | _ => impossible "Typecheck.exptype -- bad expression"

and ruleType(RULE(pat,exp),occ,loc) =  
 let val occ = Abstr occ
     val (pat',pty) = patType(pat,lamdepth occ,loc)
     val (exp',ety) = expType(exp,occ,loc)
  in (RULE(pat',exp'),pty,pty --> ety)
 end

and matchType(l,occ,loc) =
      case l
       of [] => impossible "empty rule list in typecheck.matchType"
        | [rule] => 
             let val (rule0,argt,rty) = ruleType(rule,occ,loc)
              in ([rule0],argt,rty)
             end
        | rule::rest =>
   	     let val (rule0,argt,rty) = ruleType(rule,occ,loc)
   	         fun checkrule rule' =
   		    let val (rule1,argt',rty') = ruleType(rule',occ,loc)
   		     in unifyTy(rty, rty')
   		        handle Unify(mode) =>
   		         (err loc COMPLAIN ("rules don't agree (" ^ mode ^ ")")
			   (fn ppstrm =>
   			    (PPType.resetPPType();
   			     add_newline ppstrm; add_string ppstrm "expected: ";
			     ppType ppstrm rty; add_newline ppstrm;
   			     add_string ppstrm "found:    ";
			     ppType ppstrm rty'; add_newline ppstrm;
   			     add_string ppstrm "rule:"; add_break ppstrm (1,2);
   			     ppRule ppstrm (rule',!printDepth))));
                        rule1
   		    end
   	      in (rule0::(map checkrule rest),argt,rty)
   	     end

and decType0(decl,occ,loc) : dec =
    case decl
      of VALdec vbs =>
	   let fun vbType(vb as VB{pat, exp, tyvars=(tv as (ref tyvars))}) =
	        let val (pat',pty) = patType(pat,infinity,loc)
		    and (exp',ety) = expType(exp,occ,loc)
		 in unifyTy(pty,ety)
		     handle Unify(mode) =>
		       (err loc COMPLAIN
			 ("pattern and expression in val dec don't agree ("
			  ^ mode ^ ")")
			 (fn ppstrm =>
		          (PPType.resetPPType();
		           add_newline ppstrm; add_string ppstrm "pattern:    ";
			   ppType ppstrm pty; add_newline ppstrm;
			   add_string ppstrm "expression: ";
			   ppType ppstrm ety; add_newline ppstrm;
			   add_string ppstrm "in declaration:";
			   add_break ppstrm (1,2);
			   ppVB ppstrm (vb,!printDepth))));
		    generalizePat(pat,tyvars,occ,loc);
                    VB{pat=pat',exp=exp',tyvars=tv}
                end
	    in VALdec(map vbType vbs)
	   end
       | VALRECdec(rvbs) =>
 	   let fun setType(RVB{var=VALvar{typ,...}, resultty=NONE, ...}) =
		     typ := mkRefMETAty(1+ lamdepth occ)
		 | setType(RVB{var=VALvar{typ,...}, resultty=SOME ty, ...}) =
		     typ := ty
		 | setType _  = impossible "typecheck.783"
	       fun rvbType(rvb as RVB{var=v as VALvar{typ,...},
				      exp,resultty,tyvars}) =
		    let val (exp',ety) = expType(exp,Abstr(Rator occ),loc)
		     in unifyTy(!typ, ety)
		        handle Unify(mode) =>
		          (err loc COMPLAIN
			    ("pattern and expression in val rec dec don't agree ("
			     ^ mode ^ ")")
			    (fn ppstrm =>
		             (PPType.resetPPType();
		              add_newline ppstrm; add_string ppstrm "pattern:    ";
			      ppType ppstrm (!typ); add_newline ppstrm;
		              add_string ppstrm "expression: ";
			      ppType ppstrm ety; add_newline ppstrm;
		              add_string ppstrm "in declaration:";
			      add_break ppstrm (1,2);
		              ppRVB ppstrm (rvb,!printDepth))));
                        RVB{var=v,exp=exp',resultty=resultty,tyvars=tyvars}
		    end
                 | rvbType _ = impossible "typecheck.786"
                  
 	       fun genType(RVB{var,tyvars = ref tyvars,...}) =
			 generalizeTy(var,tyvars,occ,loc)
               val _ = (app setType rvbs)
               val rvbs' = map rvbType rvbs
               val _ = (app genType rvbs)
	    in VALRECdec rvbs'
	   end
       | EXCEPTIONdec(ebs) =>
	   let fun checkWeak(VARty(ref(OPEN{kind=UBOUND _,weakness,...}))) = 
	             if  weakness = infinity
		         then err loc COMPLAIN
			      "non-weak type variable in exception declaration"
			      nullErrorBody
		     else if weakness > generalize_point occ
		         then err loc COMPLAIN
			      "type variable in exception type not weak enough"
			      nullErrorBody
		     else if weakness <= 0
			 then err loc COMPLAIN
			      "type variable in top level exception type"
			      nullErrorBody
		     else ()
		 | checkWeak(CONty(_,args)) =
		     app checkWeak args
		 | checkWeak _ = ()
	       fun ebType(EBgen{etype=SOME ty,...}) = checkWeak(ty)
	         | ebType _ = ()
            in app ebType ebs;
               decl
	   end
       | LOCALdec(decIn,decOut) =>
	   let val decIn' = decType0(decIn,LetDef occ,loc)
               val decOut' = decType0(decOut,occ,loc)
            in LOCALdec(decIn',decOut')
           end
       | SEQdec(decls) => 
           SEQdec(map (fn decl => decType0(decl,occ,loc)) decls)
       | ABSTYPEdec{abstycs,withtycs,body} => 
	     let fun makeAbstract(GENtyc{stamp,arity,eq,path,kind}) =
		     (kind := ABStyc(GENtyc{stamp=stamp,arity=arity,path=path,
					    eq=eq,kind=ref(!kind)});
		      eq := NO)
                   | makeAbstract _ = impossible "typecheck.718"
                 val body'= decType0(body,occ,loc)
              in app makeAbstract abstycs;
		 ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=body'}
	     end
       | MARKdec(dec,a,b) => MARKdec(decType0(dec,occ,(a,b)),a,b)
       | _ => decl

val _ = resetOverloaded()
val dec' = decType0(dec, if toplev then Root else (LetDef Root), loc);
val _ = resolveOverloaded env

in dec'
end

end (* structure Typecheck *)
