(*$MonoSet: MONO_SET EQUALITY List *)
(* NB. This functor is needed to compile the Make system itself,
       so the above tag declaration is unnecessary. *)

loadSig "EQUALITY";
loadSig "MONO_SET";

functor MonoSet (
  structure Element: EQUALITY
): MONO_SET =

(* MONOMORPHIC SETS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           23 Jan 1991

Maintenance:    Author


DESCRIPTION

   A straightforward implementation in terms of lists.  A more efficient
   version could use elements with an ordering function as well as equality.


SEE ALSO

   EqSet, Set.


RCS LOG

$Log: MonoSet.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.7  1991/10/22  18:33:26  db
Added map, apply, fold and fold' functions.

Revision 1.6  91/02/12  17:19:34  17:19:34  db (Dave Berry)
This is really embarrassing!  I had implemented set equality as
list equality, but although the lists have no repeated elements
they can be in arbitrary order.  I've fixed this.

Revision 1.5  91/02/12  12:56:02  12:56:02  db (Dave Berry)
Changed datatype to abstype.

Revision 1.4  91/02/11  20:30:33  20:30:33  db (Dave Berry)
The ELEMENT signature has been renamed EQUALITY, as part of the major
reorganisation of the library.
Also reorganised the presentation.

Revision 1.3  91/01/25  15:45:26  db
Removed reference to CoreUtils.member, which has been redefined.

Revision 1.2  91/01/24  17:25:11  17:25:11  db (Dave Berry)
Removed version value.

Revision 1.1  91/01/24  15:40:01  15:40:01  db (Dave Berry)
Initial revision


*)

struct


(* ABSTYPE *)

  abstype Set = Set of Element.T list
  with


(* TYPES *)

    type Element = Element.T

    type T = Set


(* LOCAL *)

    fun memberEq _  _ [] = false
    |   memberEq eq x (h::t) =
	  eq x h orelse memberEq eq x t

    fun dropRepeats _ []  = []
    |   dropRepeats _ [x] = [x]
    |   dropRepeats eq (x::xs) =
          if memberEq eq x xs then dropRepeats eq xs
          else x :: (dropRepeats eq xs)


(* CONSTANTS *)

    val empty = Set []


(* CREATORS *)

    fun singleton elem = Set [elem]


(* CONVERTERS *)

    fun list (Set l) = l

    fun fromList l = Set (dropRepeats Element.eq l)


(* OBSERVERS *)

    fun isEmpty (Set []) = true
    |   isEmpty _ = false

    fun member elem (Set []) = false
    |   member elem (Set (h::t)) =
	  Element.eq elem h orelse member elem (Set t)

    fun size (Set l) = CoreUtils.length l

    local
      fun allContained [] _ = true
      |   allContained (h::t) s =
            member h s andalso allContained t s
    in
      fun eq (s1 as Set l) s2 =
            size s1 = size s2 andalso
            allContained l s2
    end


(* SELECTORS *)

    exception Empty of string

    fun select (Set []) = raise Empty "select"
    |   select (Set (h::t)) = (h, Set t)


(* MANIPULATORS *)

    fun insert elem (s as Set l) =
	  if member elem s then s
	  else Set (elem :: l)

    fun intersect s (Set []) = empty
    |   intersect s (Set (h::t)) =
          if member h s
          then insert h (intersect s (Set t))
          else intersect s (Set t)

    local
      fun partition' (f, Set [], yes, no) = (yes, no)
      |   partition' (f, Set (h::t), yes, no) =
            if f h
	    then partition' (f, Set t, insert h yes, no)
            else partition' (f, Set t, yes, insert h no)
    in
      fun partition f s = partition' (f, s, empty, empty)
    end

    fun remove elem set =
          #1 (partition (fn a => not (Element.eq elem a)) set)

    fun difference s (Set []) = s
    |   difference s (Set (h::t)) =
          let val s' = remove h s
          in difference s' (Set t)
	  end

    fun union (Set l1) (Set l2) = Set (dropRepeats Element.eq (l1 @ l2))

    local
      fun closure' ([], f, result) = result
      |   closure' (h::t, f, result) =
            let val more = f h
                val (new as Set l) = difference more result
            in closure' (t @ l, f, union result new)
            end
    in
       fun closure f (s as Set l) = closure' (l, f, s)
    end


(* ITERATORS *)

    fun map f (Set l) = Set (List.map f l)

    fun apply f (Set l) = List.apply f l


(* REDUCERS *)

    fun fold f base (Set l) = List.foldL f base l

    fun fold' f (Set []) = raise Empty "fold'"
    |   fold' f (Set l)  = List.foldL' f l

  end (* abstype *)

end;
