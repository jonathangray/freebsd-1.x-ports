(* Copyright 1990 by AT&T Bell Laboratories *)
(* fixity.sig *)

signature FIXITY =
sig

    datatype fixity = NONfix | INfix of (int*int)

    val infixleft : int -> fixity
    val infixright : int -> fixity
    val fixityToString : fixity -> string
end
