(*$REAL: GeneralTypes *)

signature REAL =
sig

(* REALS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Nov 1989

Maintenance:	Author


DESCRIPTION

   Standard functions on the built-in type "real".

   This signature is based on the draft ISO/ANSI Language Compatible
   Arithmetic Standard, Version 2.2, ANSI X3T2/89-179, 
   ISO/IEC JTC1.SC22/WG11 N144


NOTES

   The arithmetic exceptions are defined to raise Overflow for all
   overflow operations and Div for attempts to divide by zero.

   The "trunc" function is intended for use when printing reals, but this might
   be an inefficient approach.  It does reduce the number of converters.

   Possible extra functions (from the Hope+ library):
   arccos, arccosh, arcsin, arcsinh, arctanh, cosh, sinh, tan, tanh, log10,
   log2, exp10, exp2

RCS LOG

$Log: REAL.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.12  91/09/13  16:41:53  16:41:53  db (Dave Berry)
Added dependency on GeneralTypes.

Revision 1.11  91/03/06  16:29:43  16:29:43  db (Dave Berry)
Added print function(s).

Revision 1.10  91/02/22  16:44:20  16:44:20  db (Dave Berry)
Renamed **! exception to Power.

Revision 1.9  91/02/12  12:18:51  12:18:51  db (Dave Berry)
Changed type to eqtype.

Revision 1.8  91/02/04  15:38:50  15:38:50  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.7  91/01/31  17:48:48  17:48:48  db (Dave Berry)
Added type.

Revision 1.6  91/01/30  18:07:42  18:07:42  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/25  19:30:52  19:30:52  db (Dave Berry)
Added dependence on OBJECT, fixed include specification.

Revision 1.4  91/01/25  19:02:53  19:02:53  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  16:55:33  16:55:33  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:26  17:08:26  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:54:46  16:54:46  db (Dave Berry)
Initial revision


*)

(* PERVASIVES *)

  eqtype real

  exception Overflow
  and Div

  val + : real * real -> real
  val - : real * real -> real
  val * : real * real -> real
  val / : real * real -> real
  val ~ : real -> real
  val abs: real -> real
  val floor: real -> int
  val sin: real -> real
  val cos: real -> real
  val arctan: real -> real
  val exp: real -> real

  exception Ln
  val ln: real -> real

  exception Sqrt
  val sqrt: real -> real


(* SYSTEM *)

  val radix: int
   (* radix; the "base" of the implementation. *)

  val precBits: int Option
   (* precBits; the number of radix digits provided by the implementation.
      This is None if the system supports arbitrary precision real numbers. *)

  val minExp: int Option
   (* minExp; the smallest exponent value, in terms of the radix. *)

  val maxExp: int Option
   (* maxExp; the largest exponent value, in terms of the radix. *)

  val denorm: bool
   (* denorm; true if the implementation support denormalised values,
      false otherwise. *)

  val minReal: real Option
   (* minReal; the smallest real that can be stored on the system, or None
      if the system supports arbitrary length reals. *)

  val minNormReal: real Option
   (* minNormReal; the smallest normalised real that can be stored on the
      system, or None if the system supports arbitrary length reals.
      If denorm = false then minNormReal = minReal. *)

  val maxReal: real Option
   (* maxReal; the largest real that can be stored on the system, or None
      if the system supports arbitrary length reals. *)

  val precReal: int Option
   (* precReal; the largest (decimal) precision that a real can have on the
      system, or None if the system supports reals with arbitrary precision. *)

  val epsilon: real Option
   (* epsilon; the largest relative representation error for the set of
      normalised values provided by the implementation. *)

(* CONSTANTS *)

  val pi: real
   (* pi = 3.14159265 *)

  val e: real
   (* e = 2.71828183 *)


(* TYPES *)

  eqtype T
     sharing type T = real


(* OBSERVERS *)

  val lt: real -> real -> bool

  val le: real -> real -> bool

  val gt: real -> real -> bool

  val ge: real -> real -> bool

  val eq: real -> real -> bool

  val ne: real -> real -> bool

  val fixedWidth: bool
   (* fixedWidth = false *)


(* CONVERTERS *)

  val string: real -> string
   (* string n; returns the string representation of n, in the most convenient
      form.  The result must be a real constant as defined in The Definition Of
      Standard ML. *)

  val stringNoE: real -> string
  (* stringNoE n; returns the string representation of n, without exponent.
     The results alway contains a decimal point and at least one digit after
     the decimal point. *)

  val stringE: real -> string
  (* stringE n; returns the string representation of n, with exponent.  If all
     digits after the decimal point are 0, they are omitted, as is the decimal
     point itself.  0.0 is printed "0.0". *)

  val stringPadE: int -> real -> string
  (* stringPadE w n; as stringE n except that the exponent must contain
     at least w characters. *)

  val print: outstream -> real -> unit
   (* print os n; sends the string representation of n to the stream os,
      in the most convenient form.  The output must be a real constant as
      defined in The Definition Of Standard ML. *)

  val printNoE: outstream -> real -> unit
  (* printNoE os n; sends the string representation of n to the stream os,
     without exponent.  The results alway contains a decimal point and
     at least one digit after the decimal point. *)

  val printE: outstream -> real -> unit
  (* printE os n; sends the string representation of n to the stream os,
     with exponent.  If all digits after the decimal point are 0, they are
     omitted, as is the decimal point itself.  0.0 is printed "0E0". *)

  val printPadE: outstream -> int -> real -> unit
  (* printPadE os w n; as printE os n except that the exponent must contain
     at least w characters. *)

  val trunc: int -> real -> real
  (* trunc p n; returns n with (decimal) precision p.  If p is greater than
     the existing precision, trunc has no effect. *) 

  val round: real -> int
  (* round n; returns the nearest integer to n, halves rounded up. *)

  val int: real -> int
  (* int n; returns n truncated to an integer, truncated towards 0. *)

  val ceiling: real -> int
  (* ceiling n; returns n rounded up to the next integer. *)


(* MANIPULATORS *)

  val max: real -> real -> real
   (* max x y; returns the greater of x and y.  *)

  val min: real -> real -> real
   (* min x y; returns the lesser of x and y.  *)

  val maxMin: real -> real -> real * real
   (* maxMin x y = (max (x, y), min (x, y)).  *)

  val sinCos: real -> real * real
   (* sinCos x = (sin x, cos x). *)

  (* infix 8 ** *)
  exception Power of real * int
  val ** : real * int -> real
   (* x ** y; x raised to the power y. *)
end
