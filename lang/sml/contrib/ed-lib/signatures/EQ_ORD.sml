(*$EQ_ORD *)

signature EQ_ORD =
sig

(* A TYPE WITH EQUALITY AND ORDERING FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           5 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T, an equality function and an
   ordering function.


SEE ALSO

   EQUALITY, ORDERING, EQ_PRINT, EQTYPE_ORD, FULL_ORD, OBJECT.


RCS LOG

$Log: EQ_ORD.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.1  91/02/11  18:27:22  18:27:22  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  type T


(* OBSERVERS *)

  val eq: T -> T -> bool
   (* eq x y; returns true if x and y are equal; returns false otherwise. *)

  val lt: T -> T -> bool
   (* lt x y; returns true if x is less than y; returns false otherwise. *)

end;

