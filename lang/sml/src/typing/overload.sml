(* Copyright 1989 by AT&T Bell Laboratories *)
(* overload.sml *)

structure Overload : OVERLOAD = struct

open ErrorMsg Variables Types TypesUtil Unify BasicTypes

type subst = (tyvar * tvinfo) list

exception SoftUnify

local fun typeArgs n = 
	    if n>0
	    then (mkRefMETAty 0) :: typeArgs(n-1)
	    else []
 in fun copyScheme (tyfun as TYFUN{arity,...}) : ty * ty =
	let val tvs = typeArgs arity
	 in (applyTyfun(tyfun,tvs),
	     if arity>1 then tupleTy tvs else hd tvs)
	end
end

fun rollBack subst =
    let fun loop (nil,trace) = trace
	  | loop (((tv as ref kind),oldkind)::subst,trace) =
	       (tv := oldkind;
		loop(subst,(tv,kind)::trace))
     in loop(subst,nil)
    end

fun redoSubst nil = ()
  | redoSubst ((tv as ref(OPEN{kind=META, ...}),INSTANTIATED ty)::rest) =
      (tv := INSTANTIATED ty; redoSubst rest)
  | redoSubst (_) = impossible "Overload--redoSubst"

fun softUnify(ty1: ty, ty2: ty): subst =
    let val subst: subst ref = ref nil
	fun softInst(tv as ref info: tyvar, ty: ty) : unit =
	    let fun scan(ty: ty) : unit =  (* simple occurrence check *)
		   case ty
		     of VARty(tv') => 
		          if eqTyvar(tv, tv') then
			      raise SoftUnify
			  else
			      (case tv'
				  of ref(OPEN{kind=FLEX fields,...}) =>
				      app (fn (_,ty') => scan ty') fields
				   | _ => ()
			      )
		      | CONty(_, args) => app scan args
		      | ty => ()  (* propagate error *)
	     in case info
		  of OPEN{kind=META,...} => ()
		   | _ => raise SoftUnify;
 	        scan ty;
		subst := (tv, info)::(!subst);
		tv := INSTANTIATED ty
	    end
	
	fun unify(ty1: ty, ty2: ty): unit =
	    let val ty1 = prune ty1
		and ty2 = prune ty2
	     in case (ty1,ty2)
		  of (WILDCARDty, _) => ()  (* wildcards unify with anything *)
		   | (_, WILDCARDty) => ()  (* wildcards unify with anything *)
		   | (VARty(tv1),VARty(tv2)) =>
		       if eqTyvar(tv1,tv2) then () else softInst(tv1,ty2)
		   | (VARty(tv1),_) => softInst(tv1,ty2)
		   | (_,VARty(tv2)) => softInst(tv2,ty1)
		   | (CONty(tycon1, args1), CONty(tycon2, args2)) =>
		       if eqTycon(tycon1, tycon2)
		       then unifyLists(args1, args2)
		       else (unify(reduceType ty1, ty2)
			     handle ReduceType => 
			       unify(ty1, reduceType ty2)
			       handle ReduceType => raise SoftUnify)
		   | _ => raise SoftUnify
	    end
	
	and unifyLists([],[]) = ()
	  | unifyLists(ty1::rest1, ty2::rest2) = 
	      (unify(ty1,ty2); unifyLists(rest1,rest2))
	  | unifyLists(_) = raise SoftUnify

     in unify(ty1,ty2)
	  handle SoftUnify => (rollBack(!subst); raise SoftUnify);
	!subst
    end

exception Overld

val overloaded = ref (nil: (var ref * ErrorMsg.complainer * ty) list)

fun resetOverloaded () = overloaded := nil

fun pushOverloaded (refvar as ref(OVLDvar{options,scheme,...}), err) = 
	   let val (scheme',ty) = copyScheme(scheme)
	    in overloaded := (refvar,err,ty) :: !overloaded;
	       scheme'
	   end
  | pushOverloaded _ = impossible "overload.1"

fun resolveOverloaded env  =
 let fun resolveOVLDvar(rv as ref(OVLDvar{name,options,...}),err,context) =
	(let fun findFirst({indicator, variant}::rest) =
		 ((softUnify(applyPoly(indicator,Root), context), variant, rest)
		   handle SoftUnify => findFirst(rest))
	       | findFirst(nil) = 
		   (err COMPLAIN "overloaded variable not defined at type"
		     (fn ppstrm =>
		       (PPType.resetPPType();
			PrettyPrint.add_newline ppstrm;
			PrettyPrint.add_string ppstrm "symbol: "; 
			PPUtil.ppSym ppstrm name;
			PrettyPrint.add_newline ppstrm;
			PrettyPrint.add_string ppstrm "type: ";
			PPType.ppType env ppstrm context));
		    raise Overld)
	     fun findSecond({indicator, variant}::rest) =
		 ((rollBack(softUnify(applyPoly(indicator,Root), context));
		   err COMPLAIN
		     ("overloaded variable cannot be resolved: "^Symbol.name(name))
		     nullErrorBody;
		   raise Overld)
		  handle SoftUnify => findSecond(rest))
	       | findSecond(nil) = ()
	     val (subst,var,restOptions) = findFirst(!options)
	     val subst = rollBack subst
	  in findSecond(restOptions);
	     redoSubst subst;
	     rv := var
	 end handle Overld => ())
       | resolveOVLDvar _ = impossible "overload.2"

  in app resolveOVLDvar (!overloaded); 
     overloaded := nil
 end

end (* structure Overload *)
