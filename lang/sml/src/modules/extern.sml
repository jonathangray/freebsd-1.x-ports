(* Copyright 1992 by AT&T Bell Laboratories *)

(***************************************************************************
 
  EXTERN.SML: externalize sharings on a structure already defined in a 
  signature. It is used by instantiate.sml when part of the signature is
  already instantiated (parent for an argument signature - argument for a
  functor body signature).

 ***************************************************************************)

signature EXTERN = sig
  val name_A: Symbol.symbol
  val name_P: Symbol.symbol
  val name_X: Symbol.symbol
  val name_O: Symbol.symbol
  val name_B: Symbol.symbol
  val hidden: Symbol.symbol -> bool
  val externalize_sharing: 
    Symbol.symbol -> Modules.Structure
    -> Modules.Signature -> Modules.Signature
  val update_structure:
    Symbol.symbol -> Modules.Structure -> Modules.Structure -> unit
  val make_argument :
        {parent:Modules.Structure, parameter:Modules.Structure}
         -> Modules.Structure
end;

structure Extern: EXTERN = struct
open Symbol Access Modules ModuleUtil ErrorMsg TypesUtil;

val name_A = strSymbol "<Argument>"
val name_P = strSymbol "<Parent>"
val name_X = strSymbol "<Parameter>"
val name_O = strSymbol "<open>"
val name_B = strSymbol "<body>"

fun hidden s = (s=name_A) orelse (s=name_P) orelse (s=name_O)

fun externalize_sharing_str name str {internal,external} =
  fold 
    (fn ([],res) => res (* nul paths shouldn't exist anyway *)
      | (path as (sym::end_path),{internal,external})=>
       if sym = name then 
         let val STRvar{binding=new_ext,...} = lookBindingSTR (str,end_path) in
         case external
         of NONE => {internal=internal,external=SOME new_ext}
          | SOME old_ext => 
              if eqOrigin (new_ext,old_ext) then 
                {internal=internal,external=external}
              else impossible "Extern: extern_sharing_str"
         end
       else
         {internal=path::internal,external=external})
  internal {internal=[],external=external}
;         

fun externalize_sharing_tyc name str {internal,external} =
  fold 
    (fn ([],res) => res (* nul paths shouldn't exist anyway *)
      | (path as (sym::end_path),{internal,external})=>
       if sym = name then 
         let val new_ext =  lookBindingTYC (str,end_path) in
         case external
         of NONE => {internal=internal,external=SOME new_ext}
          | SOME old_ext => 
              if equalTycon (new_ext,old_ext) then 
                {internal=internal,external=external}
              else impossible "Extern: extern_sharing_tyc"
         end
       else
         {internal=path::internal,external=external})
  internal {internal=[],external=external}
;         

fun externalize_sharing name parent
    (SIG{stamp,symbols,path,env, 
         kind=ref (TOP{strcount,fctcount,typecount,slotcount,
                       sConstraints,tConstraints})}) =
      SIG{
        stamp=stamp,env=env,symbols=symbols,path=path,
        kind=ref (TOP{strcount=strcount,fctcount=fctcount,
                      typecount=typecount,slotcount=slotcount,
                      sConstraints= 
                        map (externalize_sharing_str name parent) sConstraints,
                      tConstraints=
                        map (externalize_sharing_tyc name parent) tConstraints
                })}
  | externalize_sharing _ _ ERROR_SIG = ERROR_SIG
  | externalize_sharing _ _ _ = impossible "Extern: externalize_sharing"


fun update_structure name str arg =
  case arg
  of INSTANCE{sign as SIG{env,...},subStrs,...} => ((
       case Env.look(!env,name)
       of (STRbind (STRvar {binding=STR_FORMAL {pos, ...},...})) =>
	     Array.update(subStrs,pos,str)
	| _ => impossible "Extern: update_structure 1")
       handle Env.Unbound => 
	 if name = name_A then () else impossible "Extern: update_structure 3")
   | ERROR_STR => ()
   | INSTANCE{sign as ERROR_SIG,...} => ()
   | _ => impossible "Extern: update_structure 2"


fun make_argument {parent,parameter} =
  let val binding_X = 
        STRbind(STRvar{name=name_X,access=SLOT 1,
		       binding=STR_FORMAL{pos=1,spec=FULL_SIG}})
      val binding_P = 
        STRbind(STRvar{name=name_P,access=SLOT 0,
		       binding=STR_FORMAL{pos=0,spec=FULL_SIG}})
      val env = 
        Env.bind (name_X, binding_X,
                  Env.bind (name_P, binding_P, Env.empty))
  in
  INSTANCE{
    sign=SIG{symbols = ref [name_P,name_X],
             path = NONE,
             stamp = Stamps.newFree (),
             env = ref env,
             kind = ref (TOP{strcount=2,fctcount=0,typecount=0,slotcount=0,
                             tConstraints=[],sConstraints=[]})},
    subStrs = Array.arrayoflist [parent,parameter],
    subFcts = Array.arrayoflist [],
    types = Array.arrayoflist [],
    origin = SELF(Stamps.newFree ()),
    path = []}
  end

end


