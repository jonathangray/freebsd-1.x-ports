(* Copyright 1989 by AT&T Bell Laboratories *)
(* misc.sig *)

signature MISC =
sig

  (* val ASTERISKsym : Symbol.symbol not used *)
  val EQUALsym : Symbol.symbol

  val for : 'a list -> ('a -> 'b) -> unit

  val sort3 : (Symbol.symbol * 'a * 'b) list -> (Symbol.symbol * 'a * 'b) list

  val bogusID : Symbol.symbol
  val bogusExnID : Symbol.symbol
  val bogusExp: Absyn.exp

  val anonParamName : Symbol.symbol
(* not used anymore ?
  val nullSigStamp : int
  val nullSig : Basics.Structure

  
  val nullStr : Basics.Structure
  val nullParamVar : Basics.structureVar 
*)

  val discard : 'a -> unit
  val single : 'a -> 'a list

  val varcon : Modules.binding -> Absyn.exp

  val checkbound: TyvarSet.tyvarset * Types.tyvar list * ErrorMsg.complainer -> unit
end
