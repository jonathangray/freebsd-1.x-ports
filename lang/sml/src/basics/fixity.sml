(* Copyright 1990 by AT&T Bell Laboratories *)
(* fixity.sml *)

structure Fixity : FIXITY =
struct

    datatype fixity = NONfix | INfix of (int*int)

  (* building fixities *)

    fun infixleft n = INfix (n+n, n+n+1)
    fun infixright n = INfix (n+n+1, n+n)

    fun fixityToString NONfix = "nonfix "
      | fixityToString (INfix (i,_)) =
	         (if i mod 2 = 0 then "infix " else "infixr ")^
	         (if i div 2 > 0 then makestring (i div 2)^" " else "")
end
