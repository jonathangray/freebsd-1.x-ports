(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sig *)

signature ENVIRONMENT =
sig

  type statenv  (* = StaticEnv.statenv *)
  type dynenv   (* = DynamicEnv.dynenv *)
  type invenv   (* = InverseEnv.invenv *)

  type environment (* = {static: statenv, dynamic: dynenv, inverse: invenv} *)
  type staticEnv   (* = {static: statenv, inverse: invenv} *)

  val emptyEnv : environment
  val staticPart : environment -> staticEnv
  val makeStaticEnv : statenv -> staticEnv

  val layerEnv    : environment * environment -> environment
  val concatEnv   : environment * environment -> environment
  val layerStatic : staticEnv * staticEnv -> staticEnv
  val filterEnv   : environment * Symbol.symbol list -> environment
  val filterStaticEnv   : staticEnv * Symbol.symbol list -> staticEnv
  val consolidateEnv : environment -> environment
  val consolidateStatic : staticEnv -> staticEnv

  val bindEnv : statenv * invenv * Access.lvar list
                * System.Unsafe.object vector * environment
                -> environment

  val catalogEnv : staticEnv -> Symbol.symbol list
  val describe   : staticEnv -> Symbol.symbol -> unit

  val topLevelEnvRef : environment ref  (* interactive top level env *)
  val pervasiveEnvRef : environment ref  (* pervasive environment *)

end (* structure Env *)
