(*$ORD_PRINT *)

signature ORD_PRINT =
sig

(* A TYPE WITH A PRINT FUNCTION AND AN ORDERING FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           10 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T, a function to produce a string
   representation of a value of that type, and a function to compare
   two values of that type.


SEE ALSO

   PRINT, ORDERING, EQ_ORD, EQ_PRINT, EQTYPE_PRINT, OBJECT


RCS LOG

$Log: ORD_PRINT.sml,v $
Revision 1.1  1994/02/08 00:23:28  jkh
Initial revision

Revision 1.2  91/03/06  16:29:36  16:29:36  db (Dave Berry)
Added print function(s).

Revision 1.1  91/02/11  18:55:38  18:55:38  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  type T


(* CONVERTERS *)

  val string: T -> string
   (* string x; returns the usual string representation of x. *)

  val print: outstream -> T -> unit
   (* print os x; send the usual string representation of x to
      the stream os. *)


(* OBSERVERS *)

  val fixedWidth: bool
   (* fixedWidth; is true if the usual string representation of type T uses
      a fixed number of characters for all values. *)

  val lt: T -> T -> bool
   (* lt x y; returns true if x is less than y; returns false otherwise. *)

end;

