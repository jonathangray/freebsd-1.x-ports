(* Copyright 1989 by AT&T Bell Laboratories *)
signature TYPECHECK = sig
(*   structure Env:ENV *)
  val decType : Modules.env * Absyn.dec * bool
       * (Absyn.linenum * Absyn.linenum->ErrorMsg.complainer)
       * (Absyn.linenum * Absyn.linenum)
       -> Absyn.dec
end 
