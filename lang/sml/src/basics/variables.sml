(* Copyright 1990 by AT&T Bell Laboratories *)
(* variables.sig *)

structure Variables : VARIABLES =
struct

    datatype var
      = VALvar of 		              (* ordinary variables *)
	  {access : Access.access,
	   name : Symbol.symbol list,
	   typ : Types.ty ref}
      | OVLDvar of       	      	      (* overloaded identifier *)
	  {name : Symbol.symbol,
	   options: {indicator: Types.ty, variant: var} list ref,
	   scheme: Types.tyfun}
      | ERRORvar

  (* building variables *)

    fun mkVALvar id =
	VALvar{access = Access.PATH[Access.namedLvar(id)], name = [id], 
	       typ = ref Types.UNDEFty}

end
