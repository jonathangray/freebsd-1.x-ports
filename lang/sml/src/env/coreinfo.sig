(* Copyright 1989 by AT&T Bell Laboratories *)
(* coreinfo.sig *)

signature COREINFO =
sig
    val exnBind : Types.datacon ref
    val exnMatch : Types.datacon ref

    val exnOrd : Types.datacon ref
    val exnSubscript : Types.datacon ref
    val exnRealSubscript : Types.datacon ref
    val exnRange : Types.datacon ref
     
    val stringequalPath : int list ref
    val polyequalPath : int list ref
    val registerPath : int list ref
    val getDebugVar : Variables.var ref
    val resetCore: unit -> unit
    val setCore : Modules.env * Modules.spath -> unit
    val forcerPath : int list ref
    val vector0Path : int list ref
    val corePath : int list ref
    val coreLty : (int -> Lambda.lty) ref 
    val sregisterPath : int list ref
    val errorMatchPath : Variables.var ref
end

