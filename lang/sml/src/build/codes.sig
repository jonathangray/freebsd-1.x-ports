(* Copyright 1989 by AT&T Bell Laboratories *)
signature CODEGENERATOR =
sig
 val generate : CPS.function * System.Unsafe.object option * ErrorMsg.complainer -> string
end

signature ASSEMBLER =
sig 
 val generate : (CPS.function * System.Unsafe.object option * ErrorMsg.complainer) * outstream -> unit
end
