(* Copyright 1992 by AT&T Bell Laboratories *)

signature ELABSIG =
  sig
    type context
    val elabEMBEDsig : 
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) *
      (Ast.linenum * Ast.linenum) -> context * Symbol.symbol -> Ast.sigexp
      -> Modules.Signature
    val elabFSIGB : 
      (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) * 
      (Ast.linenum * Ast.linenum) -> Modules.env * Stamps.scope ->
      Symbol.symbol * Ast.fsigexp 
       -> Modules.funsigVar list * Modules.env
    val elabPARTIALsig :
       (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) *
       (Ast.linenum * Ast.linenum) ->
       Symbol.symbol * Modules.Signature * Modules.env * Normalize.env ref *
       (unit -> Stamps.stamp) ->
       Ast.sigexp 
       -> Modules.Signature
    val elabTOPsig : 
       (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) * 
       (Ast.linenum * Ast.linenum) -> 
       Modules.binding Env.env * (unit->Stamps.stamp) * Symbol.symbol option ->
       Ast.sigexp
       -> Modules.Signature
    val make_fsigexp : 
       (Ast.linenum * Ast.linenum -> ErrorMsg.complainer) *
       (Ast.linenum * Ast.linenum) -> Symbol.symbol option ->
       context -> Ast.fsigexp
        -> Modules.FctSignature
  end



