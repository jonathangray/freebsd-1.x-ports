(* Copyright 1989 by AT&T Bell Laboratories *)
(* access.sig *)

signature ACCESS = sig

  structure P : PRIMOP
  type primop
  eqtype lvar  (* lambda variable id *)
  type slot  (* position in structure record *)
  type path  (* slot chain relative to lambda variable *)

  datatype access 
    = SLOT of slot
    | PATH of path  
    | INLINE of primop
  
  datatype conrep
      = UNTAGGED
      | TAGGED of int
      | TAGGEDREC of int * int
      | UNTAGGEDREC of int
      | CONSTANT of int
      | TRANSPARENT
      | REF
      | VARIABLE of access (* exception constructor *)
      | VARIABLEc of access (* exception constructor without argument *)

  val mkLvar : unit -> lvar
  val sameName : lvar * lvar -> unit
  val dupLvar : lvar -> lvar
  val namedLvar : Symbol.symbol -> lvar
  val lvarName : lvar -> string
  val saveLvarNames : bool ref

  val pr_lvar: lvar-> string
  and pr_slot: slot -> string
  and pr_path: path-> string
  and pr_access: access-> string

(* the different kinds of records *)
  datatype record_kind
    = RK_VECTOR
    | RK_RECORD
    | RK_SPILL
    | RK_CLOSURE
    | RK_CONT

end (* signature ACCESS *)
