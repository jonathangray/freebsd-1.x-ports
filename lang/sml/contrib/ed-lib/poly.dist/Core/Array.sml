loadLocalSig "ARRAY";

structure CoreArray: CORE_ARRAY = Array

(* CORE ARRAY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           11 Mar 1991

Maintenance:    Author


DESCRIPTION

   Poly/ML provides arrays as a built-in type.  In version 2.01 
   this matches the CORE_ARRAY signature directly.  In earlier
   versions the code commented out below should be used instead.


RCS LOG

$Log: Array.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

Revision 1.3  1991/11/18  18:16:01  db
Updated to Poly/ML version 2.01.


*)
(*
struct

  infix sub

  datatype 'a array = A of 'a Array.array ref

  exception Size = CoreVector.Size

  exception Subscript = Array.Subscript

  fun array (n, x) = A (ref (Array.array (n, x)))

  fun arrayoflist l = (A (ref (Array.arrayoflist l)))

  fun tabulate (i, f) =
        let fun tab j = if j < i then f j :: tab (j+1) else nil
        in if i < 0 then raise Size else arrayoflist (tab 0)
        end

  fun op sub (A (ref a), i) = Array.sub (a, i)

  fun update (A (ref a), i, v) = Array.update (a, i, v)

  fun length (A (ref a)) = Array.length a
end
*)
