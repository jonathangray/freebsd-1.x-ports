(*$Int : INT *)

loadSig "INT";

structure Int: INT =

(* INTEGERS FOR PPOPLOG

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		26 Mar 1991

Maintenance:	Author


DESCRIPTION

    Poplog ML supports arbitrary-precision arithmetic, and implements
    rem, quot, max, min, -- and **.


RCS LOG

$Log: Int.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.1  91/04/10  16:58:46  16:58:46  db (Dave Berry)
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
       PML.Int.quot

  val op rem =
       PML.Int.rem

  fun x quotRem y = (x quot y, x rem y)

  val max = PML.Int.max

  val min = PML.Int.min

  fun maxMin x y: int * int = if x < y then (y, x) else (x, y)

  infix 5 --
  fun x -- y = PML.List.fromto x y

  infix 8 **
  exception Power of int * int
  fun x ** y =
      PML.Int.** (x, y)
      handle PML.Int.Power => raise Power (x, y)
		
end
