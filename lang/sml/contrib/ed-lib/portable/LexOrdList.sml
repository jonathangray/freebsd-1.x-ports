(*$LexOrdList: FULL_SEQ_ORD *)

structure LexOrdList: FULL_SEQ_ORD =

(* LIST COMPARISON FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        6 Feb 1991

Maintenance:	Author 


RCS LOG

$Log: LexOrdList.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.2  91/02/22  15:34:00  15:34:00  db (Dave Berry)
Moved prefixes function to List.sml.

Revision 1.1  91/02/11  20:05:13  20:05:13  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type 'a T = 'a list


(* OBSERVERS *)

  fun lt _ _ [] = false
  |   lt _ [] _ = true
  |   lt p (h::t) (h'::t') = p h h' orelse (not (p h' h) andalso lt p t t')

  fun le _ [] _ = true
  |   le _ _ [] = false
  |   le p (h::t) (h'::t') = p h h' andalso (not (p h' h) orelse le p t t')

  fun gt _ [] _ = false
  |   gt _ _ [] = true
  |   gt p (h::t) (h'::t') = p h h' orelse (not (p h' h) andalso gt p t t')

  fun ge _ _ [] = true
  |   ge _ [] _ = false
  |   ge p (h::t) (h'::t') = p h h' andalso (not (p h' h) orelse ge p t t')

end
