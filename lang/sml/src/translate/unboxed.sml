(* unboxed.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
*)

structure Unboxed : sig
    val unboxedAssign : Types.ty -> Access.primop
    val unboxedUpdate : Types.ty -> Access.primop
  end = 

  struct
    open Access Types BasicTypes TypesUtil

    local fun getStamp (GENtyc{stamp, ...}) = stamp
       in val intStamp = getStamp intTycon
          val realStamp = getStamp realTycon
          val exnStamp = getStamp exnTycon
          val contStamp = getStamp contTycon
          val arrayStamp = getStamp arrayTycon
          val refStamp = getStamp refTycon
      end

  (* Determine the boxity of a list of constructors, given the boxity of 
   * the first constructor (boxity is represented by the flavor of update 
   * primop).
   *)
    fun constrBoxity (b, []) = b
      | constrBoxity (P.UNBOXEDUPDATE, DATACON{rep=CONSTANT _,...}::r) =
	  constrBoxity(P.UNBOXEDUPDATE, r)
      | constrBoxity (P.BOXEDUPDATE, DATACON{rep=CONSTANT _,...}::r) = P.UPDATE
      | constrBoxity (P.BOXEDUPDATE, _::r) = constrBoxity(P.BOXEDUPDATE, r)
      | constrBoxity _ = P.UPDATE

  (* Determine the boxity of a type and return the appropriate flavor of
   * update primop.
   *)
    fun updateOp ty = 
       (case (TypesUtil.headReduceType ty)
	 of CONty(GENtyc{arity, kind = ref(DATAtyc dcons), ...}, args) => 
               (case dcons
        	 of [DATACON{const=false, typ=CONty(_,[ty,_]),...}] =>
		       updateOp (applyTyfun (TYFUN{arity=arity,body=ty}, args))
		  | (DATACON{rep=CONSTANT _,...}::r) => 
                       constrBoxity(P.UNBOXEDUPDATE, r)
		  | (_::r) => constrBoxity(P.BOXEDUPDATE, r)
		  | _ => P.UPDATE  (* this case is probably a compiler bug! *)
	       )
	  | CONty(RECORDtyc [], _) => P.UNBOXEDUPDATE  (* unit type *)
	  | CONty(RECORDtyc _, _) => P.BOXEDUPDATE
		 (* Note that even records of one field are boxed *)
	  | CONty(GENtyc{stamp,...},_) =>
	      if (stamp = intStamp)
	      then P.UNBOXEDUPDATE
	      else if (stamp = realStamp) orelse (stamp = exnStamp)
	             orelse (stamp = contStamp) orelse (stamp = arrayStamp)
		     orelse (stamp = refStamp)
		   then P.BOXEDUPDATE
		   else P.UPDATE
	  | _ => P.UPDATE
       )

    fun unboxedAssign ty = 
       let val CONty(_,[ty',_]) = headReduceType ty
	   val CONty(_,[_,ty'']) = headReduceType ty'
	in updateOp ty''
       end handle Bind => P.UPDATE
  
    fun unboxedUpdate ty = 
       let val CONty(_,[ty',_]) = headReduceType ty
	   val CONty(_,[_,_,ty'']) = headReduceType ty'
	in updateOp ty''
       end handle Bind => P.UPDATE

  end  (* Unboxed *)
