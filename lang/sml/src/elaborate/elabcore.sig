(* Copyright 1992 by AT&T Bell Laboratories *)

signature ELABCORE =
  sig
    type typeEnv
    type tyvUpdate
    val elabDB :
      Modules.binding Env.env * 
     (Modules.binding Env.env * Normalize.env option) * 'a * 
     (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) ->
     Types.tyvar list * Symbol.symbol * (Symbol.symbol * Ast.ty option) list *
     (Ast.linenum * Ast.linenum) 
     -> Types.datacon list * Modules.binding Env.env
    val elabDec : 
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) * 
      (Ast.linenum * Ast.linenum -> string) * (Ast.linenum * Ast.linenum) -> 
      Modules.binding Env.env * Symbol.symbol list * Stamps.scope -> Ast.dec
      -> Absyn.dec * Modules.binding Env.env * TyvarSet.tyvarset * 
         (TyvarSet.tyvarset -> unit)
    val elabExp : 
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) *
      (Ast.linenum * Ast.linenum -> string) ->
      Ast.linenum * Ast.linenum -> Modules.binding Env.env * Stamps.scope ->
      Ast.exp 
      -> Absyn.exp * TyvarSet.tyvarset * (TyvarSet.tyvarset -> unit)
    val elabType :
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) -> 
      Ast.linenum * Ast.linenum -> typeEnv -> Ast.ty
      -> Types.ty * TyvarSet.tyvarset
    val elabTyv :
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) ->
      Ast.linenum * Ast.linenum -> Ast.tyvar
      -> Types.tyvar
    val elabTyvList : 
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) ->
      Ast.linenum * Ast.linenum -> Ast.tyvar list
      -> Types.tyvar list
  end
