(* Copyright 1992 by AT&T Bell Laboratories *)
(* env/statenv.sml *)

structure StaticEnv : STATICENV =
struct

  type statenv = Modules.env
  type binding = Modules.binding

  exception Unbound = Env.Unbound
  exception SpecialEnv = Env.SpecialEnv
  val empty = Env.empty
  val look = Env.look
  val bind = Env.bind
  val open' = Env.open'
  val special = Env.special
  val atop = Env.atop
  val consolidate = Env.consolidate
  val app = Env.app
  val map = Env.map

end  (* structure StaticEnv *)