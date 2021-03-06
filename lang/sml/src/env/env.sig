(* Copyright 1989 by AT&T Bell Laboratories *)
(* env.sig *)

signature ENV =
sig
  structure Symbol : SYMBOL
  structure FastSymbol :
    sig
      type raw_symbol
      type symbol
      val rawSymbol: int * string -> raw_symbol
      val sameSpaceSymbol : symbol -> raw_symbol -> symbol

      val varSymbol: raw_symbol -> symbol
      val tycSymbol: raw_symbol -> symbol
      val sigSymbol: raw_symbol -> symbol
      val strSymbol: raw_symbol -> symbol
      val fctSymbol: raw_symbol -> symbol
      val fixSymbol: raw_symbol -> symbol
      val labSymbol: raw_symbol -> symbol
      val tyvSymbol: raw_symbol -> symbol
      val fsigSymbol: raw_symbol -> symbol
      val var'n'fix : raw_symbol -> symbol * symbol
    end
  type 'b env
  exception Unbound  
  exception SpecialEnv
  val empty: 'b env
  val look: 'b env * Symbol.symbol -> 'b
  val bind: Symbol.symbol * 'b * 'b env -> 'b env
  val open': 'b env * ('b -> 'b) * 'b env -> 'b env
  val special: (Symbol.symbol -> 'b) * 'b env -> 'b env

  (* atop(e1,e2): place e1 on top of e2 *)

  val atop: 'b env * 'b env -> 'b env
  val consolidate: '1b env -> '1b env
  val app: (Symbol.symbol * 'b -> unit) -> 'b env -> unit
  val map: ('1b -> '1b) -> '1b env -> '1b env
end
