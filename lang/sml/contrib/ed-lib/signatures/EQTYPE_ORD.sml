(*$EQTYPE_ORD *)

signature EQTYPE_ORD =
sig

(* AN EQUALITY TYPE WITH AN ORDERING FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           10 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a eqtype T and an ordering function.


SEE ALSO

   ORDERING, EQ_ORD, EQTYPE_PRINT, FULL_ORD, OBJECT.


RCS LOG

$Log: EQTYPE_ORD.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.1  91/02/11  18:19:55  18:19:55  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  eqtype T


(* OBSERVERS *)

  val lt: T -> T -> bool
   (* lt x y; returns true is x is less than y; returns false otherwise. *)

end;

