(* Copyright 1989 by AT&T Bell Laboratories *)
(* Support for IEEE floating-point constants
 * Double precision format (for normalized numbers):
 *   Bias = 1023.
 *   Exponent = 11 bits.
 *   Range of exponent = [1..2046]
 *   Mantissa = 52 (+1) bits.
 *   Value = (-1)^s * 2^(e-1023) * 1.f
 *)
structure IEEEReal = RealConst(
struct
    val significant = 53 (* 52 + redundant 1 bit *)
    val minexp = ~1021 and  maxexp = 1024
    open Bits
    fun transreal (sign, frac, exp) =
	 if frac(0,1)=0 then "\000\000\000\000\000\000\000\000"
          else implode[chr(orb(lshift(sign,7),rshift(exp+1022,4))),
		       chr(andb(255,orb(lshift(exp+1022,4),frac(1,4)))),
		       chr(frac(5,8)),
		       chr(frac(13,8)),
		       chr(frac(21,8)),
		       chr(frac(29,8)),
		       chr(frac(37,8)),
		       chr(frac(45,8))]

end)
