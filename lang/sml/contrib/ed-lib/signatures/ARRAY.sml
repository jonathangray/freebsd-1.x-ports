(*$ARRAY: GeneralTypes *)

signature ARRAY =
sig

(* ONE DIMENSIONAL ARRAYS

Created by:	Dave Berry, LFCS, University of Edinburgh
Date:		30 Oct 1989

Maintenance:	Author


DESCRIPTION

   An array is a single-dimensional polymorphic array with elements that
   be updated in place.  Different arrays are not equal, even if they hold
   the same elements.  Arrays always admit equality.  There is only one
   empty array.  The first element of any array has the index 0.

   Elements can be updated by the "update" function; the conventional
   "a sub i := v" notation doesn't work because the "a sub i" would have to
   return a reference, and this would either cause problems for the garbage
   collector or add an extra level of indirection.  If this extra level of
   indirection is acceptable, you can create constant vectors of references
   using the Vector type.

   To create an array of arrays, either use a tabulate function
   or first create an array of empty arrays and then update each
   element with the desired initial value.

   Unlike lists, this signature doesn't provide the  subLast  and  extractLast
   functions.  The "size" function should take constant time, so this is not a
   handicap.

   The Array functions agreed by the implementors of SML/NJ, Poly/ML and
   Poplog ML are available in the structure CoreArray.  The Array.Array
   type is the same as the CoreArray.array type.

SEE ALSO

   ARRAY_PARSE, VECTOR, MONO_ARRAY.


NOTES

   A possible implementation would be to view a random access file as an
   array.


RCS LOG

$Log: ARRAY.sml,v $
Revision 1.1  1994/02/08 00:23:23  jkh
Initial revision

Revision 1.19  1991/10/22  18:20:36  db
Added note about CoreArray structure.

Revision 1.18  1991/04/25  12:25:56  db
Deleted spurious comment.

Revision 1.17  91/03/06  16:28:24  16:28:24  db (Dave Berry)
Added print function(s).

Revision 1.16  91/02/22  19:02:10  19:02:10  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.
Uncurried update to match standard agreed for arrays and vectors by the SML
  implementers, and changed the order of arguments of updateRange and copy
  to match (although these functions are still curried).

Revision 1.15  91/02/13  13:54:17  13:54:17  db (Dave Berry)
Removed some unnecessary imperative attributes from type variables.

Revision 1.14  91/02/12  19:41:05  19:41:05  db (Dave Berry)
Removed same and different; made eq and ne test for identity.

Revision 1.13  91/02/12  12:17:38  12:17:38  db (Dave Berry)
Changed type to eqtype.

Revision 1.12  91/02/11  18:06:28  18:06:28  db (Dave Berry)
Moved parse and read functions to ARRAY_PARSE.sml, and removed the sort
and comparison functions altogether, as part of the major reorganisation
of the library.

Revision 1.11  91/02/04  15:37:16  15:37:16  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.10  91/01/30  18:42:38  18:42:38  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.9  91/01/30  18:06:39  18:06:39  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.8  91/01/28  13:04:35  13:04:35  db (Dave Berry)
Removed empty constant since it can't be defined in terms of the core
primitives.
Changed same and different to require equality types as arguments,
for same reason.
Removed imperative attributes from comparison functions, because they
aren't needed.
,

Revision 1.7  91/01/26  13:40:52  13:40:52  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.6  91/01/26  13:19:48  13:19:48  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.5  91/01/25  19:03:04  19:03:04  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.4  91/01/25  16:55:41  16:55:41  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.3  91/01/24  17:08:33  17:08:33  db (Dave Berry)
Removed version value.

Revision 1.2  91/01/15  11:31:03  11:31:03  db (Dave Berry)
Renamed "empty" function to "null"; added "empty" constant.

Revision 1.1  90/12/17  16:55:50  16:55:50  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  eqtype 'a Array

  eqtype 'a T 
    sharing type T = Array


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, n); raised by the creation functions when they are called
      with a negative size argument. *)

  val create: Nat -> '_a -> '_a Array
   (* create n i; create a Array of n locations, each containing i.
      Raise (Size ("create", n)) if n < 0. *)

  val tabulate: Nat * (int -> '_a) -> '_a Array
   (* tabulate (n, f); create an array v of n locations, (v sub 0) to
      (v sub (n-1)), with (v sub i) initialised to (f i).
      Raise (Size ("tabulate", n)) if n < 0. *)

  val tabulate': Nat * ('b -> '_a * 'b) * 'b -> '_a Array
   (* tabulate' (n, f, base); create an array of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub 0) initialised to (# 1 (f base)) and
      (v sub i (i > 0)) initialised to (# 1 (f (# 2 f_i))), where f_i is
      the result of the i^th application of f.
      Raise (Size ("tabulate'", n)) if n < 0. *)


(* CONVERTERS *)

  val list: 'a Array -> 'a list
   (* list v; make a list containing (only) the elements of v, in
      the same order. *)

  val fromList: '_a list -> '_a Array
   (* fromList v; make an array containing (only) the elements of v, in
      the same order. *)

  val stringSep: string -> string -> string ->
                 ('a -> string) -> 'a Array -> string
   (* stringSep start finish sep p v; returns the string representation of v,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: ('a -> string) -> 'a Array -> string
   (* string p v; returns the canonical string representation of v. *)

  val printSep: outstream -> string -> string -> string ->
                (outstream -> 'a -> unit) -> 'a Array -> unit
   (* printSep os start finish sep p v; sends the string representation of v
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> (outstream -> 'a -> unit) -> 'a Array -> unit
   (* print os p v; sends the canonical string representation of v to
      the stream os. *)

(* OBSERVERS *)

  val isEmpty: 'a Array -> bool
   (* isEmpty v; returns true if v is empty. *)

  val size: 'a Array -> Nat
   (* size v; return the number of elements in v. *)

  val eq: ''a Array -> ''a Array -> bool
   (* eq x y; returns true if (x = y); returns false otherwise. *)

  val ne: ''a Array -> ''a Array -> bool
   (* ne x y; returns true if (x <> y); returns false otherwise. *)


(* SELECTORS *)

  exception Subscript of string * int
  (* infix 9 sub *)
  val sub: 'a Array * int -> 'a
   (* v sub n; return the n+1'th element of v.
      Raise (Subscript ("sub", n)) if not (0 <= n <= size v). *)

  val nth: int -> 'a Array -> 'a
   (* nth n v; return the n+1'th element of v.
      Raise (Subscript ("nth", n)) if not (0 <= n <= size v). *)

  exception Extract of int * int
  val extract: int -> int -> '_a Array -> '_a Array
   (* extract start finish v; returns the sub-array of v starting with
      (v sub start) and ending with (v sub (finish - 1)).
      Returns the empty array if (start = finish).
      Raise (Extract (start, finish)) if not (0 <= start,finish <= size v). *)


(* MANIPULATORS *)

  val rev: '_a Array -> '_a Array
   (* rev v; builds a new Array containing the elements of v in
      reverse order. *)

  (* infix 6 ^ *)
  val ^ : '_a Array * '_a Array -> '_a Array
   (* v ^ v'; builds an array containing the elements of v' appended to those
      of v. *)

  exception Update of int
  val update: 'a Array * int * 'a -> unit
   (* update (v, n, i); replace (v sub n) with i.
      Raise (Update n) if not (0 <= n <= size v). *)

  exception Copy of int * int * int
  val copy: '_a Array -> int -> int -> '_a Array -> int -> unit
   (* copy v start finish v' start'; copies the sub-array of v starting with
      (v sub start) and ending with (v sub (finish - 1)) to the Array v',
      starting with (v' sub start').  Has no effect if (start = finish).
      Raises (Copy (start, finish, start')) if
      not (0 <= start,finish <= size v) or if
      not (0 <= start',start'+finish-start <= size v'). *)
 
  exception UpdateRange of int * int
  val updateRange: 'a Array -> int -> int -> 'a -> unit
   (* updateRange v start finish i; update the elements of v starting with
      (v sub start) and ending with (v sub (finish - 1)) with i.  Has no effect
      if (start = finish).  Raises (UpdateRange (start, finish)) if
      not (0 <= start,finish <= size v). *)


(* REDUCERS *)

  val foldR: ('a -> 'b -> 'b) -> 'b -> 'a Array -> 'b
   (* foldR f base v; folds using f associating to the right over the
      base element.
      foldR f [a1,a2,...,an] base = f(a1,f(a2,...,f(an,base)...)). *)

  val foldL: ('a -> 'b -> 'b) -> 'b -> 'a Array -> 'b
   (* foldL f v base; folds using f associating to the left over the
      base element.
      foldL f [a1,a2,...,an] base = f(an,...,f(a2,f(a1,base))...). *)

  exception Empty of string
  val foldR': ('a -> 'a -> 'a) -> 'a Array -> 'a
   (* foldR' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldR'") if v is empty. *)

  val foldL': ('a -> 'a -> 'a) -> 'a Array -> 'a
   (* foldL' f v; folds using f associating to the left over the
      first element of v.  Raises (Empty "foldL'") if v is empty. *)

  val pairwise: ('a -> 'a -> bool) -> 'a Array -> bool
   (* pairwise f v; true if (f (v sub i) (v sub (i + 1))) is true for all
      0 <= i < size v, or if v is empty. *)


(* ITERATORS *)

  val map: ('a -> '_b) -> 'a Array -> '_b Array
   (* map f v; builds a new Array by applying f to each element of v. *)

  val apply: ('a -> unit) -> 'a Array -> unit
   (* apply f v; applies f to each element of v. *)

  val iterate: ('a * int -> '_b) -> 'a Array -> '_b Array
   (* iterate f v; builds a new Array by applying f to each element of v
      paired with its index. *)

  val iterateApply: ('a * int -> unit) -> 'a Array -> unit
   (* iterateApply f v; applies f to each element of v paired with its index. *)
end
