(*$FULL_ORD *)

signature FULL_ORD =
sig

(* A TYPE WITH ORDERING FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           17 Sep 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T and a full set of ordering functions.


SEE ALSO

   ORDERING, FULL_SEQ_ORD, SEQ_ORD, EQ_ORD, EQTYPE_ORD.


RCS LOG

$Log: FULL_ORD.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

# Revision 1.1  1991/10/22  18:24:36  db
# Initial revision
#


*)


(* TYPES *)

  type T


(* OBSERVERS *)

  val lt: T -> T -> bool
   (* lt x y; returns true if x is less than y; returns false otherwise. *)

  val le: T -> T -> bool
   (* le x y; returns true if x is less than or equal to y; returns
      false otherwise. *)

  val gt: T -> T -> bool
   (* gt x y; returns true if x is less than y; returns false otherwise. *)

  val ge: T -> T -> bool
   (* ge x y; returns true if x is less than or equal to y; returns
      false otherwise. *)

end;

