(*$EQUALITY *)

signature EQUALITY =
sig

(* A TYPE WITH AN EQUALITY FUNCTION

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Jan 1991

Maintenance:    Author


DESCRIPTION

   This signature defines a type T and an equality function.


SEE ALSO

   ORDERING, PRINT, EQ_PRINT, EQ_ORD, OBJECT, MONO_SET.


RCS LOG

$Log: EQUALITY.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.2  91/02/11  18:22:58  18:22:58  db (Dave Berry)
Changed the name of this signature from ELEMENT TO EQUALITY, as part of
the major reorganisation of the library.

Revision 1.1  91/01/23  16:45:05  16:45:05  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  type T


(* OBSERVERS *)

  val eq: T -> T -> bool
   (* eq x y; returns true if x and y are equal; returns false otherwise. *)

end;

