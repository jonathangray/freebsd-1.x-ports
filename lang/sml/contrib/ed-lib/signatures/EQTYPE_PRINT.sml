(*$EQTYPE_PRINT *)

signature EQTYPE_PRINT =
sig

(* AN EQTYPE WITH A PRINT FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           10 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines an eqtype T and a function to produce a string
   representation of a value of that type.


SEE ALSO

   PRINT, EQ_PRINT, EQTYPE_ORD, OBJECT


RCS LOG

$Log: EQTYPE_PRINT.sml,v $
Revision 1.1  1994/02/08 00:23:27  jkh
Initial revision

Revision 1.2  91/03/06  16:28:49  16:28:49  db (Dave Berry)
Added print function(s).

Revision 1.1  91/02/11  18:21:28  18:21:28  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  eqtype T


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
end;

