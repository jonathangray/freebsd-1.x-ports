(*$Int : INT *)

loadSig "INT";

structure Int: INT =

(* INTEGERS FOR POLY

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author


DESCRIPTION

   Poly/ML supports arbitrary-precision arithmetic, and implements rem and quot.


RCS LOG

$Log: Int.sml,v $
Revision 1.1  1994/02/08 00:23:14  jkh
Initial revision

Revision 1.1  91/09/13  16:23:28  16:23:28  db (Dave Berry)
Initial revision




*)

struct


(* PERVASIVES *)

  (* We implement the pervasives as they "should" be: *)

  type int = int

  exception Overflow
  and Div = Div

  val op + = op + : int * int -> int
  val op - = op - : int * int -> int
  val op * = op * : int * int -> int
  val op div = op div : int * int -> int
  val op mod = op mod : int * int -> int
  val ~ = ~ : int -> int
  val abs = abs : int -> int

  val real = real


(* SYSTEM *)

  val minInt = None

  val maxInt = None


(* TYPE *)

  type T = int


(* OBSERVERS *)

  val fixedWidth = false

  fun lt x y = (x: int) < y
  fun gt x y = (x: int) > y
  fun le x y = (x: int) <= y
  fun ge x y = (x: int) >= y
  fun eq x y = (x: int) = y
  fun ne x y = (x: int) <> y


(* CONVERTERS *)

  val string = CoreUtils.intToString

  fun print os i = output (os, string i)


(* MANIPULATORS *)

  infix 7 divMod
  fun x divMod y = (x div y, x mod y)

  infix 7 quot rem quotRem
  val op quot =
       PolyML.Alternative_Div_and_Mod.quot

  val op rem =
       PolyML.Alternative_Div_and_Mod.rem

  fun x quotRem y = (x quot y, x rem y)

  fun max x y: int = if x > y then x else y

  fun min x y: int = if x < y then x else y

  fun maxMin x y: int * int = if x < y then (y, x) else (x, y)

  infix 5 --
  fun x -- y = if x > y then nil
  		else x :: (x + 1 -- y)

  infix 8 **
  exception Power of int * int
  fun x ** 0 = 1
  |   x ** 1 = x
  |   x ** 2 = x * x
  |   x ** n =
    if n < 0 then raise Power (x, n)
    else
      let val f = if n mod 2 = 0 then 1 else x
      in ((x ** (n div 2)) ** 2) * f
      end
end
