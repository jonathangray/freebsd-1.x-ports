(* Copyright 1990 by AT&T Bell Laboratories *)
(* variables.sig *)

signature VARIABLES =
sig

    datatype var
      = VALvar of 		                 (* ordinary variables *)
	  {access : Access.access,
	   name : Symbol.symbol list,
	   typ : Types.ty ref}
      | OVLDvar of       	      	      (* overloaded identifier *)
	  {name : Symbol.symbol,
	   options: {indicator: Types.ty, variant: var} list ref,
	   scheme: Types.tyfun}
      | ERRORvar

    val mkVALvar : Symbol.symbol ->  var

end
