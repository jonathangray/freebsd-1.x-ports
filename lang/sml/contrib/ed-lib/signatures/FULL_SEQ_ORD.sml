(*$FULL_SEQ_ORD *)

signature FULL_SEQ_ORD =
sig

(* A PARAMETERISED TYPE WITH ORDERING FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           17 Sep 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type 'a T and a full set of ordering functions.


SEE ALSO

   SEQ_ORD, FULL_ORD, ORDERING


RCS LOG

$Log: FULL_SEQ_ORD.sml,v $
Revision 1.1  1994/02/08 00:23:27  jkh
Initial revision

# Revision 1.1  1991/10/22  18:25:34  db
# Initial revision
#


*)


(* TYPES *)

  type 'a T


(* OBSERVERS *)

  val lt: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* lt p x y; returns true if x is less than y, using p to compare elements
      when necessary; returns false otherwise. *)

  val le: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* le p x y; returns true if x is less than or equal to y, using p to
      compare elements when necessary; returns false otherwise. *)

  val gt: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* gt p x y; returns true if x is less than y, using p to compare elements
      when necessary; returns false otherwise. *)

  val ge: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* ge p x y; returns true if x is less than or equal to y, using p to
      compare elements when necessary; returns false otherwise. *)

end;

