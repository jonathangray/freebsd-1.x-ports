loadLocalSig "ARRAY";

structure CoreArray: CORE_ARRAY =

(* CORE ARRAY FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   An array is implemented portably as a vector of references.
   Most compilers will implement them more efficiently.


RCS LOG

$Log: Array.sml,v $
Revision 1.1  1994/02/08 00:23:22  jkh
Initial revision

Revision 1.2  91/01/28  10:38:50  db
Removed command to load Vector - all Core files are loaded in the correct
order by the library build files, so this call was redundant.

Revision 1.1  91/01/25  11:29:09  11:29:09  db (Dave Berry)
Initial revision


*)

struct

  infix sub

  datatype 'a array = Array of 'a ref CoreVector.vector

  exception Size = CoreVector.Size

  exception Subscript = CoreVector.Subscript

  fun array (n, x) = Array (CoreVector.tabulate (n, fn _ => ref x))

  fun arrayoflist l = Array (CoreVector.vector (map ref l))

  fun tabulate (n, f) =
	Array (CoreVector.tabulate (n, fn x => ref(f x)))

  fun op sub (Array a, i) = !(CoreVector.sub (a, i))

  fun update (Array a, i, v) = CoreVector.sub (a, i) := v

  fun length (Array a) = CoreVector.length a
end

