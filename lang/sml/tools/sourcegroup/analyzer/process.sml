(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ProcessFun (structure Scopes :SCOPES) = struct

structure SC = Scopes
structure MD = SC.MD
structure MN = MD.MN

val popScope = SC.popScope
val plainScope = SC.plainScope
val bindScope = SC.bindScope
val localScope = SC.localScope
val enterDef = SC.enterDef

val debug = ref false
fun d x = if (!debug) then print ("  Process: "^x^"\n") else ()

fun processDecl (shared'table :SC.modtable) (ast:MD.decl) :SC.scope =
 let
  val enterRef = SC.enterRef shared'table

  fun p_decl (sc:SC.scope) (ast:MD.decl) :unit =
   (case ast of
       (MD.ModDecl arg) => (app (enterDef sc) (map (p_binder sc) arg))
     | (MD.LocalDecl (bindings, body)) =>
         let val bs = bindScope sc in
           p_decl bs bindings; p_decl (localScope bs) body
         end
     | (MD.SeqDecl arg) => app (p_decl sc) arg
     | (MD.OpenDecl arg) => app (enterRef sc) arg
     | (MD.DeclRef arg)  => app (enterRef sc) arg)

  and p_modExp (sc:SC.scope) (ast:MD.modExp) :unit =
   (case ast of
       (MD.VarModExp arg) => (enterRef sc arg; ())
     | (MD.StructModExp arg) => p_decl (plainScope sc) arg
     | (MD.AppModExp (name, argList:(MD.modExp*bool) list)) =>
         let val ps = plainScope sc in
           enterRef sc name;
           app (fn (arg,_) => (p_modExp ps) arg) argList
         end
     | (MD.LetModExp (decl, body)) =>
         let val bs = bindScope sc in
           p_decl bs decl; p_modExp (localScope bs) body
         end
     | (MD.FctModExp {params, body}) =>
         let val ps = plainScope sc in
           app (p_fctArg ps) params;
           p_modExp ps body
         end)

  and p_binder (sc:SC.scope){name:MN.moduleName, def:MD.modExp, constraint:MD.modExp option}=
    let val (kind, _) = MN.headModule name
        val mod'def = (SOME (case constraint of NONE => def | (SOME me) => me))
    in
      p_modExpOpt sc constraint;
      p_modExp sc def;
      case kind of
         MN.functorMod => (name, MD.MI {def=(SOME def),constraint=constraint})
       | _ => (name, MD.MI {def=mod'def,constraint=NONE})
    end

  and p_fctArg (sc:SC.scope)(nameOpt:MN.moduleName option, modExp:MD.modExp) =
   (case nameOpt of
       NONE =>
         (case modExp of
             (MD.VarModExp modname) => (enterRef sc modname; ())
           | (MD.StructModExp arg) => p_decl sc arg
           | _ => p_modExp sc modExp)
     | (SOME name) =>
         (p_modExp sc modExp;
          enterDef sc (name, MD.MI{def=(SOME modExp),constraint=NONE});
          ()))
  
  and p_modExpOpt (sc:SC.scope) (arg :MD.modExp option) :unit =
   (case arg of NONE => () | SOME ast => p_modExp sc ast)

  val scope = SC.sourceScope ()
 in
   p_decl scope ast; scope
 end

end
