(*$Real : REAL *)

loadSig "REAL";

structure Real: REAL =

(* REALS FOR POLY/ML

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@uk.ac.ed.lfcs
Date:		8 Nov 1989

Maintenance:	Author


DESCRIPTION

   We can use the Poplog ML makestring function to implement our string function.


NOTES

   Conservative default values have been given for the system-specific
   values, but these shouldn't be relied on.


RCS LOG

$Log: Real.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.1  91/04/10  16:59:15  16:59:15  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type T = real


(* CONSTANTS *)

  val pi = PML.Real.pi

  val e = 2.71828183



(* OBSERVERS *)

  fun lt x y = (x: real) < y

  fun gt x y = (x: real) > y

  fun le x y = (x: real) <= y

  fun ge x y = (x: real) >= y

  fun eq x y = (x: real) = y

  fun ne x y = (x: real) <> y

  val fixedWidth = false


(* SYSTEM *)

  val radix = 2

  val precBits = Some 21
  
  val minExp = Some ~61
  
  val maxExp = Some ~64

  val denorm = false


  val minReal = Some 2.16840434E~19

  val minNormReal = Some 2.16840434E~19

  val maxReal = Some 1.844673528E19

  val precReal = Some 6

  val epsilon = Some 0.00000095367431640625


(* MANIPULATORS *)

  val max = PML.Real.max

  val min = PML.Real.min

  fun maxMin x y: real * real = if x > y then (x, y) else (y, x)

  fun sinCos x = (sin x, cos x)

  infix 8 **
  exception Power of real * int
  fun x ** 0 = 1.0
  |   x ** 1 = x
  |   x ** 2 = x * x
  |   x ** n =
    if n < 0 then raise Power (x, n)
    else
      let val f = if n mod 2 = 0 then 1.0 else x
      in ((x ** (n div 2)) ** 2) * f
      end
      handle Prod => raise Power (x, n)


(* CONVERTERS *)

  val string = makestring: real -> string

  fun stringNoE _ = raise General.NotImplemented "Real.stringNoE"

  fun stringPadE _ = raise General.NotImplemented "Real.stringPadE"

  fun stringE _ = raise General.NotImplemented "Real.stringE"

  fun print os r = output (os, makestring (r: real))

  fun printNoE _ = raise General.NotImplemented "Real.printNoE"

  fun printPadE _ = raise General.NotImplemented "Real.printPadE"

  fun printE _ = raise General.NotImplemented "Real.printE"

  val round = PML.Real.round

  fun ceiling r = floor (r + 1.0)

  fun int r =
	if r < 0.0 then ~ (floor (~ r)) else floor r

  fun trunc i r =
	let val m = 10.0 ** i
	in (real (int (r * m))) / m
	end


(* PERVASIVES *)

  type real = real

  exception Overflow
  and Div = Div

  val op + = op + : real * real -> real
  val op - = op - : real * real -> real
  val op * = op * : real * real -> real
  val op / = op / : real * real -> real
  val ~ = ~ : real -> real

  val abs = abs : real -> real
  val floor = floor

  val sin = sin
  val cos = cos
  val arctan = arctan

  val exp = exp

  exception Ln = Ln
  val ln = ln

  exception Sqrt
  val sqrt = sqrt

end
