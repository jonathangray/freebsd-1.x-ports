(* Copyright 1990 by AT&T Bell Laboratories *)
signature EQTYPES =
sig
  val eqAnalyze : Modules.Structure * (Stamps.stamp -> bool) * ErrorMsg.complainer
                         -> unit
  val defineEqTycon : (Types.tycon -> Types.tycon) -> Types.tycon -> unit
  val checkEqTySig : Types.ty * Types.polysign -> bool
  val isEqTycon : Types.tycon -> bool
  val isEqType : Types.ty -> bool
end

