signature A = sig
  type t
  val f:unit -> t
 end

signature GS = sig
  structure AS:A
  val x:AS.t
 end

functor G (AW:A) :GS = struct
  structure AS:A=AW
  val x:AW.t = AS.f()
 end

funsig FS (AP:A) = GS

functor AA :FS = G
