(* Copyright 1989 by AT&T Bell Laboratories *)
(* misc.sml *)

structure Misc : MISC =
struct

  open Variables Modules Types
  open ErrorMsg Symbol PrintUtil Access BasicTypes
       TypesUtil Absyn 

  val ASTERISKsym = Symbol.varSymbol "*"
  val EQUALsym = Symbol.varSymbol "="
 
  fun for l f = app f l

  local fun uniq ((a0 as (a,_,_))::(r as (b,_,_)::_)) = 
		    if Symbol.eq(a,b) then uniq r else a0::uniq r
	  | uniq l = l
      fun gtr((a,_,_),(b,_,_)) = 
		     let val a' = Symbol.name a and b' = Symbol.name b
		         val zero = ord "0" and nine = ord "9"
			 val a0 = ordof(a',0) and b0 = ordof(b',0)
		      in if a0 >= zero andalso a0 <= nine
			  then if b0 >= zero andalso b0 <= nine
				 then size a' > size b' orelse
					  size a' = size b' andalso a' > b'
				 else false
			  else if b0 >= zero andalso b0 <= nine
				then true
				else a' > b'
		     end
   in val sort3 = uniq o Sort.sort gtr
  end

  (* following could go in Absyn *)
  val bogusID = Symbol.varSymbol "*bogus*"
  val bogusExnID = Symbol.varSymbol "*Bogus*"
  val bogusExp = VARexp(ref(VALvar{name=[bogusID],typ=ref WILDCARDty,
			           access=PATH[0]}),NONE)

  val anonParamName = Symbol.strSymbol "<AnonParam>"
  fun discard _ = ()

  fun single x = [x]

  fun varcon (VARbind v) = VARexp(ref v,NONE)
    | varcon (CONbind d) = CONexp(d, NONE)
    | varcon _ = impossible "Misc.varcon"

  fun checkbound(used,bound,err) =
    let open TyvarSet
	val boundset = fold (fn (v,s) =>
				union_tyvars(singleton_tyvar v,s,err))
		            bound no_tyvars
	fun nasty(ref(INSTANTIATED(VARty v))) = nasty v
	  | nasty(ubound as ref(OPEN{kind=UBOUND _,...})) = 
	     err COMPLAIN ("unbound type variable in type declaration: " ^
			   (PPType.tyvar_printname ubound))
	         nullErrorBody
	  | nasty _ = impossible "Misc.checkbound"
     in
	app nasty (get_tyvars(diff_tyvars(used, boundset, err)))
    end

end (* structure Misc *)
