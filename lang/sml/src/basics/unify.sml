(* Copyright 1990 by AT&T Bell Laboratories *)

structure Unify: UNIFY =
struct

(*** type unification ***)

open Types ErrorMsg TypesUtil List2

exception Unify of string


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

fun ltLabel(l1,l2) = Symbol.name l1 < Symbol.name l2

fun is_ubound (UBOUND _) = true
  | is_ubound _ = false;

(*
 * tycon_eqprop tycon:
 *
 *    This function returns the eqprop of tycon for use in determining
 * when a CONty is an equality type.
 *
 * Note: Calling this function on ERRORtyc produces an impossible
 * because an ERRORtyc should never occur in a CONty and hence an eqprop
 * of one of them should never be needed.
 *
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities.  (Ex: first argument must be an eq type but not
 * necessarily the second)  Because of this, it is currently necessary to
 * expand DEFtyc's before checking for equality types.
 *)
fun tycon_eqprop (GENtyc{eq,...}) = !eq
  | tycon_eqprop (RECORDtyc _)  = YES
  | tycon_eqprop _ = impossible "tycon_eqprop"

(*
 * fieldwise just1 just2 combine fields1 fields2:
 *
 *    This function merges two sorted lists of (label, type) pairs
 * (sorted by label) into a single sorted list of (label, type) pairs.
 * If (l1,t1) occurs in fields1 but l1 doesn't occur in fields2 then
 * (l1, just1 t1) occurs in the output.  Similarly with just2.
 * If (l, t1) occurs in fields1 and (l,t2) in fields2, then 
 * (l, combine t1 t2) occurs in the output.
 *)
fun fieldwise _ just2 _ [] fields2 = map (fn (n,t) => (n,just2 t)) fields2
  | fieldwise just1 _ _ fields1 [] = map (fn (n,t) => (n,just1 t)) fields1
  | fieldwise just1 just2 combine ((n1,t1)::r1) ((n2,t2)::r2) =
	if eqLabel(n1,n2) then
	    (n1,combine(t1,t2))::(fieldwise just1 just2 combine r1 r2)
	else if ltLabel(n1,n2) then
	    (n1,just1 t1)::(fieldwise just1 just2 combine r1 ((n2,t2)::r2))
	else
	    (n2,just2 t2)::(fieldwise just1 just2 combine ((n1,t1)::r1) r2);


(*************** adjust functions *****************************************)

fun adjust_variable eq depth weakness
		    (var2 as ref(OPEN{depth=d,eq=e,weakness=w,kind=k})) =
	((if is_ubound k then
	    (if weakness<w then
		raise Unify "weakness violation"
	    else if eq andalso not e then
		raise Unify "equality type required"
	    else
		()
	    )
	else
	    ()
	);
        var2 := OPEN{depth=min(depth,d), eq=eq orelse e,
			 weakness=min(weakness,w), kind=k})
   | adjust_variable _ _ _ _ = impossible "adjust_variable";

fun adjust_type (var as ref(OPEN{depth,eq,weakness,...})) ty =
    let fun iter _ WILDCARDty = ()
	  | iter eq (VARty(ref(INSTANTIATED ty))) = iter eq ty
	  | iter eq (VARty(var' as ref(OPEN{kind=k,...}))) =
		(if eqTyvar(var,var') then
		    raise Unify "circularity"
		else
		    adjust_variable eq depth weakness var';
		    adjust_tvkind var k)
	  | iter eq (ty as CONty(DEFtyc _, args)) =
		iter eq (headReduceType ty)
 	  | iter eq (CONty(tycon,args)) =
		(case tycon_eqprop tycon
		   of OBJ => app (iter false) args
		    | YES => app (iter eq) args
		    | _ => if eq then
			     raise Unify "equality type required"
			 else
			     app (iter false) args
		)
	  | iter _ _ = impossible "adjust_type"
    in iter eq ty end
  | adjust_type _ _ = impossible "adjust_type"

and adjust_fields var fields = app (fn (l,t) => adjust_type var t) fields

and adjust_tvkind var (FLEX fields) = adjust_fields var fields
  | adjust_tvkind var _ = ()


(*************** unify functions *****************************************)

fun unifyTy(type1,type2) =
    let val type1 = prune type1
	val type2 = prune type2
    in
	(case (headReduceType type1, headReduceType type2) of
	      (WILDCARDty,_) => ()
	    | (_,WILDCARDty) => ()
	    | (VARty var1,VARty var2) =>
		unify_variables var1 type1 var2 type2
	    | (VARty var1,etype2) =>
		unify_variable var1 type2 etype2
	    | (etype1,VARty var2) =>
		unify_variable var2 type1 etype1
	    | (CONty(tycon1,args1),CONty(tycon2,args2)) =>
		if eqTycon(tycon1,tycon2) then
		    app2 unifyTy (args1,args2)
		else
		    raise Unify "tycon mismatch"
	    | _ => raise Unify "type mismatch"
        )
    end

and unify_variables (var1 as ref(OPEN{depth=d1,eq=e1,weakness=w1,
				      kind=k1}))
		    type1
		    (var2 as ref(OPEN{depth=d2,eq=e2,weakness=w2,
				      kind=k2}))
		    type2 =
	(if eqTyvar(var1,var2) then
	     ()
	 else (
	     adjust_type var1 (VARty var2);
	     adjust_type var2 (VARty var1);
	     let val new_info = OPEN{depth = min(d1,d2),
				     eq = e1 orelse e2,
				     weakness = min(w1,w2),
				     kind = unify_tvinfos(k1,k2)};
	     in
			(*
			 * This case is to prevent ever instantiating
			 * a UBOUND variable in unifying.  This is
			 * because certain other parts of the compiler
			 * depend on this not happening.
			 *)
		case k1
		   of UBOUND _ => (var1 := new_info;
				   var2 := INSTANTIATED type1)
		    | _        => (var1 := INSTANTIATED type2;
				   var2 := new_info)
	     end
	 )
	)
   | unify_variables _ _ _ _ = impossible "unify_variables"

and unify_variable (var as ref(OPEN{kind=META,...})) ty _ =
	(adjust_type var ty; var := INSTANTIATED ty)
  | unify_variable (var as ref(OPEN{kind=FLEX fields,...})) ty
		   (CONty(RECORDtyc field_names, field_types)) =
	let val record_fields = zip2 (field_names,field_types)
	in
	    adjust_type var ty;
	    merge_fields false true fields record_fields;
	    var := INSTANTIATED ty
	end
  | unify_variable _ _ _ = raise Unify "type mismatch"

and unify_tvinfos(META,kind) = kind
  | unify_tvinfos(kind,META) = kind
  | unify_tvinfos(FLEX fields1, FLEX fields2) =
	FLEX (merge_fields true true fields1 fields2)
  | unify_tvinfos _ = raise Unify "bound type var"

(*
 * merge_fields extra1 extra2 fields1 fields2:
 *
 *    This function merges the 2 sorted field lists.  Fields occuring
 * in both lists have their types unified.  If a field occurs in only
 * one list, say fields{i} then if extra{i} is true, an Unify error
 * is raised.
 *)
and merge_fields extra1 extra2 fields1 fields2 =
	let fun extra allowed = fn t => if not allowed then
					     raise Unify "record labels"
					else
					     t
	in
	    fieldwise (extra extra1)
		      (extra extra2)
		      (fn (t1,t2) => (unifyTy(t1,t2); t1))
		      fields1 fields2
	end

end (* structure Unify *)
