(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure Hasher :HASHER = struct

fun hasher (s :string) :(string*int) = 
 let val len = size s
 in
  if len = 0 then ("",0) else
    let val a = len - 1
	val b = len div 2
	val c = b div 2
	val d = b + c
        fun h place shift = Bits.lshift(Bits.andb(63,ordof(s,place)), shift)
        val hashvalue = (h a 0)+(h b 6)+(h c 12)+(h d 18)+(h 0 24)
    in
      (s, hashvalue)
    end
 end

end
