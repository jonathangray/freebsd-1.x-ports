(*$MONO_VECTOR: GeneralTypes *)

signature MONO_VECTOR =
sig

(* CONSTANT VECTORS OF A NAMED TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		31 Oct 1989

Maintenance:	Author


DESCRIPTION

   A MonoVector is a single-dimensional constant monomorphic array of objects.
   The first element has the index 0.  MonoVectors are equal if they contain
   the same elements and these elements admit equality.

   To create a MonoVector of MonoVectors, use the "tabulate" functions.

   Example MonoVectors are ByteVectors and BoolVectors, which will often
   be implemented specially.  For example, a BoolVector can use one bit
   per element.


SEE ALSO

   VECTOR, MONO_ARRAY.


RCS LOG

$Log: MONO_VECTOR.sml,v $
Revision 1.1  1994/02/08 00:23:28  jkh
Initial revision

Revision 1.16  91/03/06  16:29:15  16:29:15  db (Dave Berry)
Added print function(s).

Revision 1.15  91/02/22  19:03:26  19:03:26  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.14  91/02/11  18:52:58  18:52:58  db (Dave Berry)
Renamed Object to Element, since it isn't an OBJECT anymore.
Added type synonym T.  Removed read, parse and comparison functions.
All part of the major reorganisation of the library.

Revision 1.13  91/02/04  15:38:43  15:38:43  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.12  91/01/30  18:44:04  18:44:04  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.11  91/01/30  18:07:34  18:07:34  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.10  91/01/26  13:43:44  13:43:44  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.9  91/01/26  13:19:41  13:19:41  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.8  91/01/25  19:02:41  19:02:41  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.7  91/01/25  16:57:25  16:57:25  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.6  91/01/24  17:06:37  17:06:37  db (Dave Berry)
Removed version value.

Revision 1.5  91/01/22  17:46:31  17:46:31  db (Dave Berry)
Changed from_list to FromList in the documentation of the function.

Revision 1.4  91/01/15  11:31:00  11:31:00  db (Dave Berry)
Renamed "empty" function to "null"; added "empty" constant.

Revision 1.3  91/01/10  18:01:22  18:01:22  db (Dave Berry)
Changed ge and le to expect le and ge functions as parameters instead
of lt and gt (which people found confusing).

Revision 1.2  90/12/17  17:09:22  17:09:22  db (Dave Berry)
Trivial changes to description.

Revision 1.1  90/12/17  16:52:00  16:52:00  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type Element
  type MonoVector
  type T
    sharing type T = MonoVector


(* CONSTANTS *)

  val empty: MonoVector
   (* empty; the empty vector of this type. *)


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the creation functions when they are invoked
      with a negative size. *)

  val create: Nat -> Element -> MonoVector
   (* create n i; create a MonoVector of n locations, each containing i.
      Raise (Size ("create", n)) if n < 0. *)

  val tabulate: Nat * (int -> Element) -> MonoVector
   (* tabulate n f; create a MonoVector v of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub i) initialised to (f i).
      Raise (Size ("tabulate", n)) if n < 0. *)

  val tabulate': Nat * ('b -> Element * 'b) * 'b -> MonoVector
   (* tabulate' n f base; create a MonoVector of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub 0) initialised to (# 1 (f base)) and
      (v sub i (i > 0)) initialised to (# 1 (f (# 2 f_i))), where f_i is
      the result of the i^th application of f.
      Raise (Size ("tabulate'", n)) if n < 0. *)


(* CONVERTERS *)

  val stringSep: string -> string -> string -> MonoVector -> string
   (* stringSep start finish sep p v; returns the string representation
      of v, beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: MonoVector -> string
   (* string v; returns the canonical string representation of v. *)

  val printSep: outstream -> string -> string -> string -> MonoVector -> unit
   (* printSep os start finish sep v; sends the string representation of v
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> MonoVector -> unit
   (* print os v; sends the canonical string representation of v to
      the stream os. *)

  val list: MonoVector -> Element list
   (* list v; make a list containing (only) the elements of v, in
      the same order. *)

  val fromList: Element list -> MonoVector
   (* fromList l; make a Vector containing (only) the elements of l, in
      the same order. *)


(* OBSERVERS *)

  val isEmpty: MonoVector -> bool
   (* isEmpty v; returns true if v is empty. *)

  val size: MonoVector -> Nat
   (* size v; return the number of elements in v. *)

  val eq: MonoVector -> MonoVector -> bool
   (* eq x y; returns true if (size x = size y) and for all i,
      0 <= i <= size x, (Element.eq (x sub i) (y sub i)). *)

  val ne: MonoVector -> MonoVector -> bool
   (* ne x y; returns true if (size x <> size y) and there exists an i
      such that 0 <= i <= size x and (Element.ne (x sub i) (y sub i)). *)


(* SELECTORS *)

  exception Subscript of string * int

  (* infix 9 sub *)
  val sub: MonoVector * int -> Element
   (* v sub n; return the n+1'th element of v.
      Raise (Subscript ("sub", n)) if not (0 <= n <= size v). *)

  val nth: int -> MonoVector -> Element
   (* nth n v; return the n+1'th element of v.
      Raise (Subscript ("nth", n)) if not (0 <= n <= size v). *)

  exception Extract of int * int
  val extract: int -> int -> MonoVector -> MonoVector
   (* extract start finish v; returns the sub-vector of v starting with
      (v sub start) and ending with (v sub (finish - 1)).
      Returns the empty vector if (start = finish).
      Raise (Extract (start, finish)) if not (0 <= start,finish <= size v). *)


(* MANIPULATORS *)

  val rev: MonoVector -> MonoVector
   (* rev v; builds a new MonoVector containing the elements of v in
      reverse order. *)

  (* infix 6 ^ *)
  val ^ : MonoVector * MonoVector -> MonoVector
   (* v ^ v'; builds a new MonoVector containing the elements of v' appended
      to those of v. *)


(* REDUCERS *)

  val foldR: (Element -> 'b -> 'b) -> 'b -> MonoVector -> 'b
   (* foldR f base v; folds using f associating to the right over the
      base element.
      foldR f [a1,a2,...,an] base = f(a1,f(a2,...,f(an,base)...)). *)

  val foldL: (Element -> 'b -> 'b) -> 'b -> MonoVector -> 'b
   (* foldL f v base; folds using f associating to the left over the
      base element.
      foldL f [a1,a2,...,an] base = f(an,...,f(a2,f(a1,base))...). *)

  exception Empty of string

  val foldR': (Element -> Element -> Element) -> MonoVector -> Element
   (* foldR' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldR'") if v is empty. *)

  val foldL': (Element -> Element -> Element) -> MonoVector -> Element
   (* foldL' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldL'") if v is empty. *)

  val pairwise: (Element -> Element -> bool) -> MonoVector -> bool
   (* pairwise f v; true if (f (v sub i) (v sub (i + 1))) is true for all
      0 <= i < size v, or if v is empty. *)


(* ITERATORS *)

  val map: (Element -> Element) -> MonoVector -> MonoVector
   (* map f v; builds a new vector by applying f to each element of v. *)

  val apply: (Element -> unit) -> MonoVector -> unit
   (* apply f v; applies f to each element of v. *)

  val iterate: (Element * int -> Element) -> MonoVector -> MonoVector
   (* iterate f v; builds a new vector by applying f to each element of v
      paired with its index. *)

  val iterateApply: (Element * int -> unit) -> MonoVector -> unit
   (* iterate f v; applies f to each element of v paired with its index. *)
end
