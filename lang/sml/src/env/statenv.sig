(* Copyright 1992 by AT&T Bell Laboratories *)
(* env/statenv.sig *)

signature STATICENV =
sig
  type statenv
  type binding
  exception Unbound  
  exception SpecialEnv
  val empty: statenv
  val look: statenv * Symbol.symbol -> binding
  val bind: Symbol.symbol * binding * statenv -> statenv
  val open': statenv * (binding -> binding) * statenv -> statenv
  val special: (Symbol.symbol -> binding) * statenv -> statenv
  val atop: statenv * statenv -> statenv
  val consolidate: statenv -> statenv
  val app: (Symbol.symbol * binding -> unit) -> statenv -> unit
  val map: (binding -> binding) -> statenv -> statenv
end
