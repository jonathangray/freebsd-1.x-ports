(*$Int : INT *)

loadSig "INT";

structure Int: INT =

(* INTEGERS

Created by:	Dave Berry LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author


DESCRIPTION

   SML/NJ implements arithmetic operators as they "should" be.


RCS LOG

$Log: Int.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  14:19:23  14:19:23  db (Dave Berry)
Initial revision



*)

struct


(* PERVASIVES *)

  type int = int

  exception Overflow = Overflow
  and Div = Div

  val op + = op + : int * int -> int
  val op - = op - : int * int -> int
  val op * = op * : int * int -> int
  val abs = abs : int -> int
  val ~ = ~ : int -> int
  val op div = op div
  val op mod = op mod

  val real = real


(* TYPE *)

  type T = int


(* SYSTEM *)

  val minInt = Some ~1073741822

  val maxInt = Some 1073741822


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

  val op quot = op quot

  val op rem = op rem

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
    handle
      Prod => raise Power (x, n)
end
