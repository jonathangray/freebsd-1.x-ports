loadLocalSig "ARRAY";

structure CoreArray: CORE_ARRAY =

(* CORE ARRAY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           11 Mar 1991

Maintenance:    Author


DESCRIPTION

   Poplog ML provides arrays as a built-in type.  Eventually the built-in
   version will match the CORE_ARRAY signature directly.  For now
   we still have to create the CoreArray structure explicitly.


RCS LOG

$Log: Array.sml,v $
Revision 1.1  1994/02/08 00:23:16  jkh
Initial revision

Revision 1.3  91/04/10  17:00:19  17:00:19  db (Dave Berry)
Arrays for Poplog


*)

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

  fun op sub (A r, i) = Array.sub (!r, i)

  fun update (A r, i, v) = Array.update (!r, i, v)

  fun length (A r) = Array.length (!r)
end

