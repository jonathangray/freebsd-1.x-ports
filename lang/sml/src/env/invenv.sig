(* Copyright 1992 by AT&T Bell Laboratories *)
(* env/invenv.sig *)

signature INVERSE_ENV =
sig
  type componentId
  type invenv
  exception Unbound
  exception SpecialEnv
  val empty : invenv
  val special : (Access.lvar -> componentId) * invenv -> invenv
  val bind : Access.lvar * componentId * invenv -> invenv
  val look : invenv -> Access.lvar -> componentId
  val atop : invenv * invenv -> invenv
  val remove: Access.lvar list * invenv -> invenv
  val consolidate : invenv -> invenv
end
