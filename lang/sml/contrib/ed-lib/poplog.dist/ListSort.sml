(*$ListSort : LIST_SORT List ListPair General *)

loadSig "LIST_SORT";

structure ListSort: LIST_SORT =

(* FUNCTIONS FOR SORTING AND PERMUTING LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        26 Mar 1991

Maintenance:	Author 


DESCRIPTION

   Poplog ML provides a sort function.


RCS LOG

$Log: ListSort.sml,v $
Revision 1.1  1994/02/08 00:23:16  jkh
Initial revision

Revision 1.1  91/04/10  16:58:54  16:58:54  db (Dave Berry)
Initial revision



*)

struct


(* MANIPULATORS *)

  fun sort p l = PML.List.sort (General.uncurry p) l

  local
    fun plug a [] = [[a]]
    |   plug a (l as x::xs) =
        (a::l) :: (map (fn l => x :: l) (plug a xs))
  in
    fun perms [] = [[]]
    |   perms (x::xs) =
	  List.foldR (General.curry op @) [] (map (plug x) (perms xs))
  end

end
