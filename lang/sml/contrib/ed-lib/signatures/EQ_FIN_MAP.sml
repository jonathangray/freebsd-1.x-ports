(*$EQ_FIN_MAP *)

signature EQ_FIN_MAP =
sig

(* FINITE MAPS

Created by:	Nick Rothwell, LFCS, University of Edinburgh
		nick@lfcs.ed.ac.uk
Date:		18 Feb 91

Maintenance:	Author


DESCRIPTION

   Finite maps from 'a to 'b; requires equality on the 'a's.


RCS LOG

$Log: EQ_FIN_MAP.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.2  91/04/11  11:14:16  11:14:16  db (Dave Berry)
Clarified comment to mergeMap.

Revision 1.1  91/03/08  17:02:47  17:02:47  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  type (''a, 'b) Map


(* VALUES *)

  val empty : (''a, 'b) Map
	(* Empty map. *)


(* CREATORS *)

  val singleton : ''a * 'b -> (''a , 'b) Map
	(* Map containing a single element. *)


(* OBSERVERS *)

  val isEmpty: (''a, 'b) Map -> bool
	(* Empty test on maps. *)

  val lookup  : (''a, 'b) Map -> ''a -> 'b Option
	(* Look up an element in a map - may fail, returning None. *)

  val dom        : (''a, 'b) Map -> ''a list
	(* Domain of a map. Might contain duplicates. *)

  val range      : (''a, 'b) Map -> 'b list
	(* Range of a map.  Might contain elements mapped to by
	   duplicate keys. *)


(* MANIPULATORS *)

  val add	 : (''a * 'b) -> (''a, 'b) Map -> (''a, 'b) Map
	(* Add an element to a map, rendering any existing mapping from that
	   value unavailable. *)

  val plus       : (''a, 'b) Map -> (''a, 'b) Map -> (''a, 'b) Map
	(* Add two maps together. Entries in the second map override entries
	   on the first one (cf. the various plus operations in the SML
	   semantics). *)

  val composeMap : ('b -> 'c) -> (''a, 'b) Map -> (''a, 'c) Map
	(* Is this an appropriate name? Apply a function to all elements of
	   the range. *)

  val fold       : (('a * 'b) -> 'b) -> 'b -> (''d, 'a) Map -> 'b
	(* Rather like the list fold operation - operates on the range of
	   a map. Order of elements not guaranteed. Also, suffers from the
	   duplicate problem above. *)

  val fold'       : (((''a * 'b) * 'c) -> 'c)-> 'c -> (''a, 'b) Map -> 'c
	(* More complex fold, with a function from (dom, range) element pairs
	   to some arbitrary type. *)

  val mergeMap:
    (('b * 'b) -> 'b) -> (''a, 'b) Map -> (''a, 'b) Map -> (''a, 'b) Map
	(* Merges two finite maps, with a composition function to apply
	   to the range elements of domain elements which clash.  In such
	   a case the first argument to the compose function is the range
	   element of the first map argument to mergeMap.  *)
end;
