(*$EQ_PRINT *)

signature EQ_PRINT =
sig

(* A TYPE WITH A PRINT FUNCTION AND AN EQUALITY FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           5 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T and a function to produce a string
   representation of a value of that type.


SEE ALSO

   PRINT, EQUALITY, EQTYPE_PRINT, EQ_ORD, OBJECT.


RCS LOG

$Log: EQ_PRINT.sml,v $
Revision 1.1  1994/02/08 00:23:25  jkh
Initial revision

Revision 1.3  91/03/06  16:28:51  16:28:51  db (Dave Berry)
Added print function(s).

Revision 1.2  91/02/12  12:38:37  12:38:37  db (Dave Berry)
Removed reference to old functor SequenceToEqPrint from SEE ALSO section.
(In fact this functor used to be called SequenceToObject.)

Revision 1.1  91/02/11  18:28:15  18:28:15  db (Dave Berry)
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

  val eq: T -> T -> bool
   (* eq x y; returns true if x and y are equal; returns false otherwise. *)

end;

