(* Copyright 1992 by AT&T Bell Laboratories *)

(* Utility functions to build absyn from ast *)

signature ELABUTIL =
  sig
    val CONSexp : Absyn.exp
    val CONSpat : Absyn.pat -> Absyn.pat
    val FALSEexp : Absyn.exp
    val FALSEpat : Absyn.pat
    val NILexp : Absyn.exp
    val NILpat : Absyn.pat
    val SELECTORexp : Symbol.symbol -> Absyn.exp
    val TRUEexp : Absyn.exp
    val TRUEpat : Absyn.pat
    val TUPLEexp : Absyn.exp list -> Absyn.exp
    val TUPLEpat : Absyn.pat list -> Absyn.pat
    val WHILEexp : Absyn.exp * Absyn.exp -> Absyn.exp
    val IFexp : Absyn.exp * Absyn.exp * Absyn.exp -> Absyn.exp
    val unitExp : Absyn.exp
    val unitPat : Absyn.pat
    val bindVARp : Absyn.pat list * ErrorMsg.complainer -> Modules.env
    val checkUniq : ErrorMsg.complainer * string -> Symbol.symbol list -> unit
    val clean_pat : ErrorMsg.complainer -> Absyn.pat -> Absyn.pat
    val completeMatch : string -> Absyn.rule list -> Absyn.rule list
    val completeMatch' :
       Absyn.rule -> Absyn.rule list -> Absyn.rule list
    val makeAPPpat : ErrorMsg.complainer -> Absyn.pat * Absyn.pat -> Absyn.pat
    val makeHANDLEexp : Absyn.exp * Absyn.rule list -> Absyn.exp
    val makeLAYEREDpat : Absyn.pat * Absyn.pat * ErrorMsg.complainer -> Absyn.pat
    val makeRECORDexp : 
      (Symbol.symbol * Absyn.exp) list * ErrorMsg.complainer -> Absyn.exp
    val makeRECORDpat :
      (Symbol.symbol * Absyn.pat) list * bool * ErrorMsg.complainer -> Absyn.pat
    val makeTB :
      Types.tyvar list * Symbol.symbol * 
      (Types.ty * TyvarSet.tyvarset) * ErrorMsg.complainer -> 
      bool -> 'a * Symbol.symbol list 
      -> Absyn.tb list * Modules.binding Env.env
    val makeTYPEdec : 
      (Absyn.tb list * Modules.binding Env.env) * ErrorMsg.complainer
       -> Absyn.dec * Modules.binding Env.env
    val pat_id : 
      Modules.spath * Modules.binding Env.env * ErrorMsg.complainer -> Absyn.pat
    val sortRecord :
      (Symbol.symbol * 'a) list * ErrorMsg.complainer -> (Symbol.symbol * 'a) list
    val FUNdec :
      (Absyn.fb list * (Ast.linenum * Ast.linenum -> string)
      * (Ast.linenum * Ast.linenum))-> Absyn.dec
  end
