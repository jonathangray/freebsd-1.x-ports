(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure HasherFileNames :HASHER = struct

fun hasher (s :string) :(string*int) = 
 let val len = size s
 in
  if len = 0 then ("",0) else
    let val last = max (0, len - 1)
	val a = max (0, last-4)
	val b = max (0, last-8)
	val c = len div 2
	val d = (len div 4) + c
        fun h place shift = Bits.lshift(Bits.andb(63,ordof(s,place)), shift)
        val hashvalue = (h a 0)+(h b 6)+(h c 12)+(h d 18)+(h 0 24)
    in
      (s, hashvalue)
    end
 end

end
