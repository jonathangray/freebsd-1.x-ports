(*$SEQ_ORD *)

signature SEQ_ORD =
sig

(* A TYPE THAT TAKES ONE PARAMETER, WITH AN ORDERING FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           10 Feb 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type 'a T and an ordering function.


SEE ALSO

   SEQUENCE, ORDERING, FULL_SEQ_ORD.


RCS LOG

$Log: SEQ_ORD.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.1  91/02/11  19:24:33  19:24:33  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  type 'a T


(* OBSERVERS *)

  val lt: ('a -> 'a -> bool) -> 'a T -> 'a T -> bool
   (* lt p x y; returns true if x is less than y, using p to compare elements
      when necessary. *)

end;

