(* Copyright 1989 by AT&T Bell Laboratories *)
(* dynenv.sig *)

signature DYNENV =
sig
  type object (* = System.Unsafe.object *)
  type dynenv
  exception Unbound  
  exception SpecialEnv
  val empty: dynenv
  val special: (Access.lvar -> object) * dynenv -> dynenv
  val look: dynenv -> Access.lvar -> object
  val bind: Access.lvar * object * dynenv -> dynenv
  val atop: dynenv * dynenv -> dynenv
    (* atop(e1,e2): place e1 on top of e2 *)
  val remove: Access.lvar list * dynenv -> dynenv
  val consolidate: dynenv -> dynenv
end
