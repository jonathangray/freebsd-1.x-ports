(*$MONO_SET *)

signature MONO_SET =
sig

(* SETS OF A GIVEN TYPE

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Jan 1991

Maintenance:    Author


DESCRIPTION

   Monomorphic sets.


SEE ALSO

   EQ_SET, SET.


RCS LOG

$Log: MONO_SET.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.4  1991/10/22  18:22:35  db
Added map, apply, fold and fold' functions.

Revision 1.3  91/09/13  16:54:47  16:54:47  db (Dave Berry)
Added type T.

Revision 1.2  91/01/24  17:31:40  17:31:40  db (Dave Berry)
Removed version value.

Revision 1.1  91/01/22  18:52:51  18:52:51  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  type Element
  type Set
  type T
    sharing type T = Set


(* CONSTANTS *)

  val empty: Set
   (* empty; the empty set. *)


(* CREATORS *)

  val singleton: Element -> Set
   (* singleton x; returns the set containing only x. *)


(* CONVERTERS *)

  val list: Set -> Element list
   (* list s; return a list of the elements of s. *)

  val fromList: Element list -> Set
   (* fromList l; return the set of elements of l, removing duplicates. *)


(* OBSERVERS *)

  val size: Set -> int
   (* size s; the number of elements in s. *)

  val isEmpty: Set -> bool
   (* isEmpty s; returns true if s is empty, false otherwise. *)

  val member: Element -> (Set -> bool)
   (* member x s; returns true is x is in s, false otherwise. *)

  val eq: Set -> Set -> bool
   (* eq s s'; returns true if s and s' have the same elements. *)



(* SELECTORS *)

  exception Empty of string
   (* Empty fn; raised if the function named fn is erronously applied to
      the empty set. *)

  val select: Set -> (Element * Set)
   (* select s; returns a pair consiting of an element of s and the set
      of the remaining elements. *)


(* MANIPULATORS *)

  val difference: Set -> Set -> Set
   (* difference s s'; returns the set of those elements of s that aren't
      also in s'.  *)

  val insert: Element -> Set -> Set
   (* insert x s; returns the union of s and {x}. *)

  val intersect: Set -> Set -> Set
   (* intersect s s'; returns the set of those elements that are in
      both s and s'. *)

  val remove: Element -> Set -> Set
   (* remove x s; returns the set of the elements of s with x removed. *)

  val partition: (Element -> bool) -> (Set -> Set * Set)
   (* partition p s; returns a pair of sets; the first containing the elements
      of s for which the predicate p is true, the second the elements of s
      for which p is false. *)

  val union: Set -> Set -> Set
   (* union s s'; returns the set of elements that are in either or both s
      and s'. *)

  val closure: (Element -> Set) -> Set -> Set
   (* closure f s; repeatedly applies f to elements of s and the elements
      of the results of such applications, until no further elements are
      generated. *)


(* ITERATORS *)

  val map: (Element -> Element) -> Set -> Set
   (* map f s; builds a new monoset by applying f to each element of s. *)

  val apply: (Element -> unit) -> Set -> unit
   (* apply f s; applies f to each element of s. *)


(* REDUCERS *)

  val fold: (Element -> 'b -> 'b) -> 'b -> Set -> 'b
   (* fold f s base; folds using f over the base element. *)

  val fold': (Element -> Element -> Element) -> Set -> Element
   (* fold' f s; folds using f over an arbitrary element of s.
      Raises (Empty "fold'") if s is empty. *)

end;

