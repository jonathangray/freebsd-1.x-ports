(***************************************************************************

	DEBUGMOD.SML: utility functions to debug modules and functors

 ***************************************************************************)

signature DEBUGMOD = sig

  val getFctArg : Modules.functorVar * Modules.Structure
		  -> Symbol.symbol option * Modules.Structure

  datatype fctcontext
   = FCTCONTEXT of {fct : Modules.functorVar,
		    str : Modules.Structure,
		    next_context : (unit -> fctcontext)}
   | NOFCTCONTEXT

  val deabstyc : (unit -> fctcontext) -> Types.tycon -> Types.tycon
  val deabsstr : (unit -> fctcontext) -> Modules.Structure -> Modules.Structure

end

structure DebugMod:DEBUGMOD = struct

local 
  open Modules Extern SigMatch ErrorMsg Stamps
in

(* RECONSTRUCTION OF THE FUNCTOR ARGUMENT *)

(* GetFctArg: get the constrained actual argument for the application 
              and the argument name *)

fun getFctArg (FCTvar{binding=fct,...},str) =
  let (* gives back NONE for anonymous functor parameter the name otherwise *)
      fun normalize_name name = if name = name_X then NONE else SOME name
      (* extract the parent, the argument signature and the parameter name
	 of a functor by going up to the original definition *)
      fun functor_origin (fct as FCT{argument,parent,paramName,...}) = 
	    (parent,argument,normalize_name paramName)
	  (* The recursive call shouldn't be necessary because it is an
	     invariant in the present state that there is only one level
	     of functor instance *)
	| functor_origin (FCT_INSTANCE{fct,...}) = functor_origin fct
	| functor_origin _ = impossible "functor_origin"
      val (parent,arg_sig,paramName) = functor_origin fct
      fun error _ msg = impossible ("getFctArg: error during match:"^msg)
      (* build the argument pair *)
      val argument =   make_argument{parent=parent,parameter=str}
      (* The stamp scope is wrong but abstract and self are false.
	 err is the off_line error message but useless because we know
	 we have already matched this
	 It is also why we don't need an environment *)
      val (coerced_argument,_) =
             match{abstract=false, self=false, err=error, arg_option=NONE,
                   printEnv=Env.empty, scope=freeScope, spath=[],
                   str=argument, sign=arg_sig}
      (* extract the parameter from an argument pair *)
      fun extract_parameter (INSTANCE{sign as SIG{env,...},subStrs,...}) = ((
	    case Env.look(!env,name_X)
	    of (STRbind (STRvar {binding=STR_FORMAL {pos, ...},...})) =>
		  Array.sub(subStrs,pos)
	       | _ => impossible "getFctArg 1")
	    handle Env.Unbound => impossible "getFctArg 3")
        | extract_parameter _ = impossible "getFctArg 2"
      val coerced_param = extract_parameter coerced_argument
  in (paramName, coerced_param) end

(* DEABSTRACTION OF THE FUNCTOR BODY *)

(* Context for deabstraction *)
datatype fctcontext
  = FCTCONTEXT of {fct : functorVar,
		   str : Structure,
		   next_context : (unit -> fctcontext)}
  | NOFCTCONTEXT

(* type deabstraction *)
fun deabstyc next_context tyc = tyc

(* structure deabstraction *)
fun deabsstr next_context str = str

end
end
