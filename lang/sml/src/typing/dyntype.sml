(* Copyright 1992 by AT&T Bell Laboratories *)

structure DynType :
sig
   datatype fncontext =
       FNCONTEXT of Types.ty * (unit -> fncontext) * (unit -> fnarg)
     | NOFNCONTEXT
   and fnarg =
       FNARG of Types.ty * (unit -> fncontext) * (Types.tycon -> Types.tycon)
     | NOFNARG

   val dynType: Types.ty * (unit -> fncontext) * 
                (Types.tycon -> Types.tycon)  -> Types.ty
end =
struct
   open ErrorMsg Types TypesUtil

   datatype fncontext =
       FNCONTEXT of Types.ty * (unit -> fncontext) * (unit -> fnarg)
     | NOFNCONTEXT
   and fnarg =
       FNARG of Types.ty * (unit -> fncontext) * (Types.tycon -> Types.tycon)
     | NOFNARG

fun makeMap deabstyc =
let val l = ref (nil : (tyvar * tyvar) list)
    fun find a =
      let fun f nil = NONE
	    | f ((key,data)::r) = if key=a then SOME data else f r
      in case a 
	 of (ref (b as OPEN{kind=META,...})) =>
	     (case f (!l)
	      of NONE => let val newRef = ref b
		         in l := ((a,newRef) :: (!l));
			    newRef
		         end
	       | SOME data => data)
 	  | (ref (OPEN{kind=UBOUND _,depth,weakness,eq,...})) =>
	     (case f (!l) 
	      of NONE => let val newRef = ref (OPEN{kind=META,depth=depth,
						    weakness=weakness,eq=eq})
			 in l := ((a,newRef) :: (!l));
			    newRef
			 end
	       | SOME data => data)
	  | _ => a
      end
    fun mapTy ty =
	case ty
	of VARty (ref (INSTANTIATED ty)) => mapTy ty
	 | VARty tyvar => VARty(find tyvar)
         | CONty (tyc,args) => mkCONty(deabstyc tyc,map mapTy args)
	 | UNDEFty => ty
	 | WILDCARDty => ty
	 | IBOUND _ =>  ty
	 | POLYty{sign,tyfun=TYFUN{arity,body},abs} =>
	      instantiateType(
	         POLYty{sign=sign,tyfun=TYFUN{arity=arity,body=mapTy body},
		     abs=abs}, Root)   (* Root may not be right! *)
in mapTy
end 

fun getMetaVars ty =
    (* Called only on mapped types. *)
   let fun f (ty,l) =
     case ty
	of VARty (ref (INSTANTIATED ty)) => f(ty,l)
	 | VARty (r as ref (OPEN{kind=META,...})) =>
	     if List.exists (fn a=>a=r) l then l else r :: l
	 | VARty _ => l
         | CONty (tyc,args) => fold f args l
	 | UNDEFty => l
	 | WILDCARDty => l
	 | POLYty _ =>  impossible "Typing.Dyntype.getMetaVars saw POLYty"
	 | IBOUND _ => impossible "Typing.Dyntype.getMetaVars saw IBOUND"
   in f (ty,nil)
   end

fun disjoint(nil,_) = true
  | disjoint(_,nil) = true
  | disjoint(h::t,l) =
	if List.exists (fn a=>h=a) l then false else disjoint(t,l)


fun dynType(ty:ty, encfn: unit -> fncontext,deabstyc:tycon -> tycon) : ty =
  let val mapTy = makeMap deabstyc
      fun loop(ty:ty,encfn:unit -> fncontext) =
	  let val ty_meta_vars = getMetaVars ty
	  in if null ty_meta_vars then
	       ty
	     else 
	       case encfn() of
		 NOFNCONTEXT => ty
	       | FNCONTEXT(ty0,encfn',getarg) =>
		    let val ty0' = mapTy ty0
		    in if disjoint(ty_meta_vars,getMetaVars ty0') then
			 loop(ty,encfn')
		       else case getarg() of
			      NOFNARG => loop(ty,encfn') (* ?? error ?? *)
			    | FNARG arg =>
				  (Unify.unifyTy(dynType arg,ty0');
				   loop(ty,encfn'))
		    end
	  end
  in loop(mapTy ty, encfn)
  end

end (* structure Dyntype *)
