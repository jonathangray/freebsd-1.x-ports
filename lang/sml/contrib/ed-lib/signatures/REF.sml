(*$REF *)

signature REF =
sig

(* REFERENCES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        3 Oct 1989

Maintenance:	Author


DESCRIPTION

   Standard functions on the built-in type "ref".

   The usual comparison operators are omitted because it's just as easy to
   write (X.lt (!x) (!y)) as (lt X.lt x y), and for numbers it's even easier
   to write (!x < !y).

   The usual conversion functions are also omitted, partly because they
   won't be needed very often and partly because there's no way to preserve
   sharing.

RCS LOG

$Log: REF.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.7  91/02/12  12:18:13  12:18:13  db (Dave Berry)
Changed type to eqtype.  This isn't good enough, but it's as close as
we can get.

Revision 1.6  91/02/11  20:43:21  20:43:21  db (Dave Berry)
Added type synonym 'a T as part of the major reorganisation of the library.

Revision 1.5  91/01/31  17:48:55  17:48:55  db (Dave Berry)
Added type.

Revision 1.4  91/01/25  16:55:36  16:55:36  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.3  91/01/24  17:08:30  17:08:30  db (Dave Berry)
Removed version value.

Revision 1.2  90/12/17  17:04:42  17:04:42  db (Dave Berry)
Fixed missing parentheses in example in Description.

Revision 1.1  90/12/17  16:54:55  16:54:55  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  (* imperative type variables here are a temporary hack for Poly *)
  val ! : '_a ref -> '_a
  val := : 'a ref * 'a -> unit


(* TYPES *)

  eqtype 'a T
    sharing type T = ref


(* OBSERVERS *)

  val eq: 'a ref -> 'a ref -> bool
   
  val ne: 'a ref -> 'a ref -> bool


(* MANIPULATORS *)

  val inc: int ref -> unit
   (* inc r; increment the contents of r. *)

  val dec: int ref -> unit
   (* dec r; decrement the contents of r. *)

  val mkRandom: int -> int -> int
   (* mkRandom seed; Given a seed, mkRandom returns a psuedo-random number
      generator which takes an integer argument of one more than the
      maximum return value required if it is positive, or one less if it
      is negative.  *) 
end
