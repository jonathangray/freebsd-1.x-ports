(*$MONO_ARRAY: GeneralTypes *)

signature MONO_ARRAY =
sig

(* ARRAYS OF A NAMED TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		12 Feb 1990

Maintenance:	Author


DESCRIPTION

   A MonoArray is a single-dimensional monomorphic array of objects that
   allows its elements to be updated in place.  The first element has the index
   0.  Different MonoArrays are not equal, even if they contain the same
   elements.  MonoArrays always admit equality.  There is one empty
   MonoArray for each application of the functor.

   To create a MonoArray of MonoArrays, use the "tabulate" functions.

   Example MonoArrays are ByteArrays and BoolArrays, which will often
   be implemented specially.  For example, a BoolArray can use one bit
   per element.


SEE ALSO

   ARRAY, MONO_VECTOR.

RCS LOG

$Log: MONO_ARRAY.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.17  91/03/06  16:29:03  16:29:03  db (Dave Berry)
Added print function(s).

Revision 1.16  91/02/22  19:02:33  19:02:33  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.
Uncurried update to match standard agreed for arrays and vectors by the SML
  implementers, and changed the order of arguments of updateRange and copy
  to match (although these functions are still curried).

Revision 1.15  91/02/12  19:41:17  19:41:17  db (Dave Berry)
Removed same and different; made eq and ne test for identity.

Revision 1.14  91/02/11  18:48:20  18:48:20  db (Dave Berry)
Renamed Object to Element, since it isn't an OBJECT anymore.
Added type synonym T.  Removed read, parse and comparison functions.
All part of the major reorganisation of the library.

Revision 1.13  91/02/04  15:38:34  15:38:34  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.12  91/01/30  18:44:01  18:44:01  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.11  91/01/30  18:07:31  18:07:31  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.10  91/01/26  13:43:29  13:43:29  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.9  91/01/26  13:19:31  13:19:31  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.8  91/01/25  19:02:31  19:02:31  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.7  91/01/25  16:57:20  16:57:20  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.6  91/01/24  17:06:34  17:06:34  db (Dave Berry)
Removed version value.

Revision 1.5  91/01/22  17:45:46  17:45:46  db (Dave Berry)
Changed from_list to FromList in the documentation of the function.

Revision 1.4  91/01/15  11:30:55  11:30:55  db (Dave Berry)
Renamed "empty" function to "null"; added "empty" constant.

Revision 1.3  91/01/10  18:01:24  18:01:24  db (Dave Berry)
Changed ge and le to expect le and ge functions as parameters instead
of lt and gt (which people found confusing).

Revision 1.2  90/12/17  17:09:07  17:09:07  db (Dave Berry)
Trivial changes to description.

Revision 1.1  90/12/17  16:50:36  16:50:36  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type Element
  type MonoArray
  type T
    sharing type T = MonoArray

(* CONSTANTS *)

  val empty: MonoArray
   (* empty; the empty array of this type. *)


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the creation functions when they are invoked
      with a negative size. *)

  val create: Nat -> Element -> MonoArray
   (* create n i; create a MonoArray of n locations, each containing i.
      Raise (Size ("create", n)) if n < 0. *)

  val tabulate: Nat * (int -> Element) -> MonoArray
   (* tabulate (n, f); create a MonoArray v of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub i) initialised to (f i).
      Raise (Size ("tabulate", n)) if n < 0. *)

  val tabulate': Nat * ('b -> Element * 'b) * 'b -> MonoArray
   (* tabulate' (n, f, base); create a MonoArray of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub 0) initialised to (# 1 (f base)) and
      (v sub i (i > 0)) initialised to (# 1 (f (# 2 f_i))), where f_i is
      the result of the i^th application of f.
      Raise (Size ("tabulate'", n)) if n < 0. *)


(* CONVERTERS *)

  val stringSep: string -> string -> string ->
		 MonoArray -> string
   (* stringSep start finish sep p v; returns the string representation of v,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: MonoArray -> string
   (* string p v; returns the canonical string representation of v. *)

  val printSep: outstream -> string -> string -> string -> MonoArray -> unit
   (* printSep os start finish sep v; sends the string representation of v
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> MonoArray -> unit
   (* print os v; sends the canonical string representation of v to
      the stream os. *)

  val fromList: Element list -> MonoArray
   (* fromList l; make an array containing (only) the elements of l, in
      the same order. *)

  val list: MonoArray -> Element list
   (* list v; make a list containing (only) the elements of v, in
      the same order. *)


(* OBSERVERS *)

  val isEmpty: MonoArray -> bool
   (* isEmpty v; returns true if v is empty. *)

  val size: MonoArray -> Nat
   (* size v; return the number of elements in v. *)

  val eq: MonoArray -> MonoArray -> bool
   (* eq x y; returns true if x and y are the same MonoArray. *)

  val ne: MonoArray -> MonoArray -> bool
   (* ne x y; returns true if x and y are not the same MonoArray. *)


(* SELECTORS *)

  exception Subscript of string * int

  (* infix 9 sub *)
  val sub: MonoArray * int -> Element
   (* v sub n; return the n+1'th element of v.
      Raise (Subscript ("sub", n)) if not (0 <= n <= size v). *)

  val nth: int -> MonoArray -> Element
   (* nth n v; return the n+1'th element of v.
      Raise (Subscript ("nth", n)) if not (0 <= n <= size v). *)

  exception Extract of int * int
  val extract: int -> int -> MonoArray -> MonoArray
   (* extract start finish v; returns the sub-vector of v starting with
      (v sub start) and ending with (v sub (finish - 1)).
      Returns the empty vector if (start = finish).
      Raise (Extract (start, finish)) if not (0 <= start,finish <= size v). *)


(* MANIPULATORS *)

  val rev: MonoArray -> MonoArray
   (* rev v; builds a MonoArray containing the elements of v in
      reverse order. *)

  (* infix 6 ^ *)
  val ^ : MonoArray * MonoArray -> MonoArray
   (* v ^ v'; builds a new MonoArray containing the elements of v' appended
      to those of v. *)

  exception Update of int
  val update: MonoArray * int * Element -> unit
   (* update (v, n, i); replace (v sub n) with i.
      Raise (Update n) if not (0 <= n <= size v). *)

  exception Copy of int * int * int
  val copy: MonoArray -> int -> int -> MonoArray -> int -> unit
   (* copy v start finish v' start'; copies the sub-vector of v starting with
      (v sub start) and ending with (v sub (finish - 1)) to the MonoArray
      v', starting with (v' sub start').  Has no effect if (start = finish).
      Raises (Copy (start, finish, start')) if
      not (0 <= start,finish <= size v) or if
      not (0 <= start',start'+finish-start <= size v'). *)

  exception UpdateRange of int * int
  val updateRange: MonoArray -> int -> int -> Element -> unit
   (* updateRange v start finish i; update the elements of v starting with
      (v sub start) and ending with (v sub (finish - 1)) with i.  Has no effect
      if (start = finish).  Raises (UpdateRange (start, finish)) if
      not (0 <= start,finish <= size v). *)


(* REDUCERS *)

  val foldR: (Element -> 'b -> 'b) -> 'b -> MonoArray -> 'b
   (* foldR f base v; folds using f associating to the right over the
      base element.
      foldR f [a1,a2,...,an] base = f(a1,f(a2,...,f(an,base)...)). *)

  val foldL: (Element -> 'b -> 'b) -> 'b -> MonoArray -> 'b
   (* foldL f v base; folds using f associating to the left over the
      base element.
      foldL f [a1,a2,...,an] base = f(an,...,f(a2,f(a1,base))...). *)

  exception Empty of string

  val foldR': (Element -> Element -> Element) -> MonoArray -> Element
   (* foldR' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldR'") if v is empty. *)

  val foldL': (Element -> Element -> Element) -> MonoArray -> Element
   (* foldL' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldL'") if v is empty. *)

  val pairwise: (Element -> Element -> bool) -> MonoArray -> bool
   (* pairwise f v; true if (f (v sub i) (v sub (i + 1))) is true for all
      0 <= i < size v, or if v is empty. *)


(* ITERATORS *)

  val map: (Element -> Element) -> MonoArray -> MonoArray
   (* map f v; builds a new vector by applying f to each element of v. *)

  val apply: (Element -> unit) -> MonoArray -> unit
   (* apply f v; applies f to each element of v. *)

  val iterate: (Element * int -> Element) -> MonoArray -> MonoArray
   (* iterate f v; builds a new vector by applying f to each element of v
      paired with its index. *)

  val iterateApply: (Element * int -> unit) -> MonoArray -> unit
   (* iterate f v; applies f to each element of v paired with its index. *)
end
