(*$Real : REAL *)

loadSig "REAL";

structure Real: REAL =

(* REALS 

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@uk.ac.ed.lfcs
Date:		8 Nov 1989

Maintenance:	Author


DESCRIPTION

   Standard functions on the built-in type "real".


NOTES

   Conservative default values have been given for the system-specific
   values, but these shouldn't be relied on.


RCS LOG

$Log: Real.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.8  91/03/06  16:38:33  16:38:33  db (Dave Berry)
Added print function(s).

Revision 1.7  91/02/22  16:43:43  16:43:43  db (Dave Berry)
Renamed **! exception to Power.

Revision 1.6  91/02/11  20:40:19  20:40:19  db (Dave Berry)
Removed Object sub-structure and comparison functions as part of the
major reorganisation of the library.
Also renamed several values and functions to give clearer names.

Revision 1.5  91/01/31  17:47:52  17:47:52  db (Dave Berry)
Added type.

Revision 1.4  91/01/30  19:01:27  19:01:27  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/25  20:19:26  20:19:26  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:25:24  17:25:24  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:02:59  15:02:59  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type T = real


(* CONSTANTS *)

  val pi = 3.14159265

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

  fun max x y: real = if x > y then x else y

  fun min x y: real = if x < y then x else y

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

  fun string _ = raise General.NotImplemented "Real.string"

  fun stringNoE _ = raise General.NotImplemented "Real.stringNoE"

  fun stringPadE _ = raise General.NotImplemented "Real.stringPadE"

  fun stringE _ = raise General.NotImplemented "Real.stringE"

  fun print _ _ = raise General.NotImplemented "Real.print"

  fun printNoE _ = raise General.NotImplemented "Real.printNoE"

  fun printPadE _ = raise General.NotImplemented "Real.printPadE"

  fun printE _ = raise General.NotImplemented "Real.printE"

  fun round r =
	let fun rnd r = floor (r + 0.5)
	in if r < 0.0 then ~ (rnd (~ r)) else rnd r
	end

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
  and Div

  val op + =
        fn (x: real, y) =>
          x + y
          handle Plus => raise Overflow
  val op - =
        fn (x: real, y) =>
          x - y
          handle Sub => raise Overflow
  val op * =
        fn (x: real, y) =>
          x * y
          handle Prod => raise Overflow
  val op / =
        fn (x: real, 0.0) =>
          raise Div
        |  (x: real, y) =>
          x / y
          handle Quot => raise Overflow
  val ~ =
        fn (x: real) =>
          ~ x
          handle Neg => raise Overflow
  val abs =
        fn (x: real) =>
          abs x
          handle Abs => raise Overflow

  val floor =
        fn x =>
          floor x
          handle Floor => raise Overflow

  val sin = sin
  val cos = cos
  val arctan = arctan

  val exp =
        fn x =>
          exp x
          handle Exp => raise Overflow

  exception Ln = Ln
  val ln = ln

  exception Sqrt
  val sqrt = sqrt

end
