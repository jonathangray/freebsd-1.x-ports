(*$PRINT *)

signature PRINT =
sig

(* A TYPE WITH A PRINT FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           5 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T and a function to produce a string
   representation of a value of that type.


SEE ALSO

   EQUALITY, ORDERING, EQ_PRINT, EQTYPE_PRINT, ORD_PRINT, OBJECT.


RCS LOG

$Log: PRINT.sml,v $
Revision 1.1  1994/02/08 00:23:29  jkh
Initial revision

Revision 1.2  91/03/06  16:29:41  16:29:41  db (Dave Berry)
Added print function(s).

Revision 1.1  91/02/11  19:21:46  19:21:46  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  type T


(* CONVERTERS *)

  val string: T -> string
   (* string x; retruns the usual string representation of x. *)

  val print: outstream -> T -> unit
   (* print os x; send the usual string representation of x to
      the stream os. *)


(* OBSERVERS *)

  val fixedWidth: bool
   (* fixedWidth; is true if the usual string representation of type T uses
      a fixed number of characters for all values. *)
end;

