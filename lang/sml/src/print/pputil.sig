(* Copyright 1989 by AT&T Bell Laboratories *)

signature PPUTIL =
sig
  structure Symbol : SYMBOL
  val ppSequence : PrettyPrint.ppstream ->
		   {sep: PrettyPrint.ppstream->unit, 
		    pr: PrettyPrint.ppstream->'a->unit,
		    style: PrettyPrint.break_style}
		   -> 'a list -> unit
  val ppClosedSequence : PrettyPrint.ppstream
			 -> {front:PrettyPrint.ppstream->unit, 
                             sep:PrettyPrint.ppstream->unit,
			     back:PrettyPrint.ppstream->unit,
                             pr:PrettyPrint.ppstream->'a->unit,
			     style:PrettyPrint.break_style}
			 -> 'a list -> unit
  val ppSym : PrettyPrint.ppstream -> Symbol.symbol -> unit
  val formatQid : Symbol.symbol list -> string
  val mlstr : string -> string
  val pp_mlstr : PrettyPrint.ppstream -> string -> unit
  val ppvseq : PrettyPrint.ppstream
               -> int -> string -> (PrettyPrint.ppstream -> 'a -> unit)
               -> 'a list -> unit
  val ppIntPath : PrettyPrint.ppstream -> int list -> unit
  val ppSymPath : PrettyPrint.ppstream -> Symbol.symbol list -> unit
end

