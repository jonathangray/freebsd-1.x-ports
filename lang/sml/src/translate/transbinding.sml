signature TRANSBINDING = 
 sig val transBindingLty : Modules.binding -> Lambda.lty
     val transStrLty : Modules.Structure -> Lambda.lty
     val transSigLty : Modules.Signature -> Lambda.lty
     val transFctLty : Modules.Functor -> Lambda.lty
     val transFsigLty : Modules.FctSignature -> Lambda.lty
 end

structure TransBinding : TRANSBINDING =
struct 

open Modules Variables Types Transtypes Lambda ErrorMsg


fun transBindingLty binding = 
  case binding
   of VARbind(VALvar{typ,...}) => transTyLty(!typ)
    | CONbind(DATACON{typ,...}) =>  transTyLty(typ)
    | SIGbind(SIGvar{binding,...}) => transSigLty(binding)
    | STRbind(STRvar{binding,...}) => transStrLty(binding)
    | FSIGbind(FSIGvar{binding,...}) => transFsigLty(binding)
    | FCTbind(FCTvar{binding,...}) => transFctLty(binding)
    | _ => impossible "transBindingLty on wrong objects"

and transBindingLty0 binding = 
  case binding
   of VARbind(VALvar{typ,...}) => [transTyLty(!typ)]
    | CONbind(DATACON{typ,...}) =>  [transTyLty(typ)]
    | SIGbind(SIGvar{binding,...}) => [transSigLty(binding)]
    | STRbind(STRvar{binding,name,...}) => 
           if Extern.hidden name then nil 
           else [transStrLty(binding)]
    | FSIGbind(FSIGvar{binding,...}) => [transFsigLty(binding)]
    | FCTbind(FCTvar{binding,...}) => [transFctLty(binding)]
    | _ => nil

and transEnvLty env = 
     let val revEnv = ModuleUtil.sortEnvBindings env
         val tyLst = map (transBindingLty0 o #2) revEnv
      in RECORDty(fold (op @) tyLst nil)
     end

and transStrLty str =
  case str 
   of SIMPLE{env,...} => transEnvLty(env)
    | INSTANCE{sign,...} => transSigLty(sign)
    | STR_OPEN{spec,...} => transSigLty(spec)
    | STR_FORMAL{spec,...} => transSigLty(spec)
    | APPLY{res,...} => transStrLty(res)
    | STR_ABSFB _ => impossible "transStrLty on STR_ABSFB objects"
    | SELF _ => impossible "transStrLty on SELF objects"
    | ERROR_STR => impossible "transStrLty on ERROR objects"

and transSigLty sign =
  case sign 
   of SIG{env,symbols,...} => 
       let fun lookup s = Env.look(!env,s)
           val tyLst = map (transBindingLty0 o lookup) (!symbols)
        in RECORDty(fold (op @) tyLst nil)
       end
    | _ => impossible "transSigLty on wrong objects"

and transFctLty fct = 
  case fct 
   of FCT{argument,body,...} => 
            ARROWty(transSigLty(argument),transStrLty(#fullstr(body)))
    | FCT_FORMAL{spec,...} => transFsigLty(spec)
    | FCT_OPEN{spec,...} => transFsigLty(spec)
    | FCT_INSTANCE{fsig,...} => transFsigLty(fsig)
    | _ => impossible "transFctLty on wrong objects"
    
and transFsigLty fsign =
  case fsign
   of FSIG{argument,body,...} => 
             ARROWty(transSigLty(argument),transSigLty(body))
    | _ => impossible "transFsigLty on wrong objects"


(****************************************************************************
 *  Turn off all effects if !System.Control.CG.representations is false     * 
 ****************************************************************************)
val flag = !System.Control.CG.representations
fun bogus1 _ = BOGUSty
fun bogus2 _ = ARROWty(BOGUSty,BOGUSty)

val transBindingLty = if flag then transBindingLty else bogus1
val transStrLty = if flag then transStrLty else bogus1
val transSigLty = if flag then transSigLty else bogus1
val transFctLty = if flag then transFctLty else bogus2
val transFsigLty = if flag then transFsigLty else bogus2

end
