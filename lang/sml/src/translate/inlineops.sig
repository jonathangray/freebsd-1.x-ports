(* INLINE_OPS:   This defines operators which may be inlined in the
   front-end by replacing them with abstract syntax.*)

signature INLINE_OPS =
   sig
      val inlsubscript : Lambda.lty -> Lambda.lexp
      val inlsubscriptv : Lambda.lty -> Lambda.lexp
      val inlupdate : Lambda.lty * Lambda.lexp -> Lambda.lexp
      val inlbyteof : unit -> Lambda.lexp
      val inlstore : unit -> Lambda.lexp
      val inlordof : unit -> Lambda.lexp
      val inlsubscriptf : unit -> Lambda.lexp
      val inlupdatef : unit -> Lambda.lexp
   end
