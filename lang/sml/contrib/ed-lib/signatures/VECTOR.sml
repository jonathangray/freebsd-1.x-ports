(*$VECTOR *)

signature VECTOR =
sig

(* CONSTANT VECTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		31 Oct 1989

Maintenance:	Author


DESCRIPTION

   A Vector is a single-dimensional constant polymorphic array.  The first
   element has the index 0.  Vectors are equal if they contain the same
   elements and these elements admit equality.

   To create a Vector of Vectors, use the "tabulate" functions.

   The Vector functions agreed by the implementors of SML/NJ, Poly/ML and
   Poplog ML are available in the structure CoreVector.  The Vector.Vector
   type is the same as the CoreVector.vector type.


SEE ALSO

   SEQ_PARSE, ARRAY.


NOTES

   A possible implementation would be a view of a random access file.
   Possibly there should be a ConstVector signature (combining the Const
   and Vector signatures) to make this easier.


RCS LOG

$Log: VECTOR.sml,v $
Revision 1.1  1994/02/08 00:23:24  jkh
Initial revision

Revision 1.12  1991/10/22  18:21:36  db
Added note about CoreVector structure.

Revision 1.11  91/03/06  16:29:52  16:29:52  db (Dave Berry)
Added print function(s).

Revision 1.10  91/02/22  19:03:24  19:03:24  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.9  91/02/11  19:37:28  19:37:28  db (Dave Berry)
Added type synonym T, string and stringSep functions, and eq and ne
functions.  This forms part of the major reorganisation of the library.

Revision 1.8  91/01/30  18:44:07  18:44:07  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.7  91/01/26  13:19:54  13:19:54  db (Dave Berry)
Renamed RefVectors to Arrays, to match common practice.

Revision 1.6  91/01/25  16:55:49  16:55:49  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.5  91/01/24  17:08:57  17:08:57  db (Dave Berry)
Removed version value.

Revision 1.4  91/01/22  17:46:35  17:46:35  db (Dave Berry)
Changed from_list to FromList in the documentation of the function.

Revision 1.3  91/01/22  17:40:29  17:40:29  db (Dave Berry)
Changed type to eqtype.

Revision 1.2  91/01/15  11:31:06  11:31:06  db (Dave Berry)
Renamed "empty" function to "null"; added "empty" constant.

Revision 1.1  90/12/17  16:58:49  16:58:49  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  eqtype 'a Vector
  eqtype 'a T
    sharing type T = Vector


(* CONSTANTS *)

  val empty: 'a Vector
   (* empty; the empty Vector. *)


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the creation functions when they are invoked
      with a negative size. *)

  val create: Nat -> 'a -> 'a Vector
   (* create n i; create a Vector of n locations, each containing i.
      Raise (Size ("create", n)) if n < 0. *)

  val tabulate: Nat * (int -> 'a) -> 'a Vector
   (* tabulate (n, f); create a Vector v of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub i) initialised to (f i).
      Raise (Size ("tabulate", n)) if n < 0. *)

  val tabulate': Nat * ('b -> 'a * 'b) * 'b -> 'a Vector
   (* tabulate' (n, f, base); create a Vector of n locations, (v sub 0) to
      (v sub (n-1)) with (v sub 0) initialised to (# 1 (f base)) and
      (v sub i (i > 0)) initialised to (# 1 (f (# 2 f_i))), where f_i is
      the result of the i^th application of f.
      Raise (Size ("tabulate'", n)) if n < 0. *)


(* CONVERTERS *)

  val stringSep: string -> string -> string ->
		 ('a -> string) -> 'a Vector -> string
   (* stringSep start finish sep p v; returns the string representation of v,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: ('a -> string) -> 'a Vector -> string
   (* string p v; returns the canonical string representation of v. *)

  val printSep: outstream -> string -> string -> string ->
                (outstream -> 'a -> unit) -> 'a Vector -> unit
   (* printSep os start finish sep p v; sends the string representation of v
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> (outstream -> 'a -> unit) -> 'a Vector -> unit
   (* print os p v; sends the canonical string representation of v to
      the stream os. *)

  val list: 'a Vector -> 'a list
   (* list v; make a list containing (only) the elements of v, in
      the same order. *)

  val fromList: 'a list -> 'a Vector
   (* fromList l; make a Vector containing (only) the elements of l, in
      the same order. *)


(* OBSERVERS *)

  val isEmpty: 'a Vector -> bool
   (* isEmpty v; returns true if v is empty. *)

  val size: 'a Vector -> Nat
   (* size v; return the number of elements in v. *)

  val eq: ('a -> 'a -> bool) -> 'a Vector -> 'a Vector -> bool

  val ne: ('a -> 'a -> bool) -> 'a Vector -> 'a Vector -> bool


(* SELECTORS *)

  exception Subscript of string * int
   (* Subscript (fn, n); raised when the function named fn is passed the
      arguments n and l such that (not (0 <= n <= length l)). *)

  (* infix 9 sub *)
  val sub: 'a Vector * int -> 'a
   (* v sub n; return the n+1'th element of v.
      Raise (Subscript ("sub", n)) if not (0 <= n <= size v). *)

  val nth: int -> 'a Vector -> 'a
   (* v sub n; return the n+1'th element of v.
      Raise (Subscript ("nth", n)) if not (0 <= n <= size v). *)

  exception Extract of int * int
  val extract: int -> int -> 'a Vector -> 'a Vector
   (* extract start finish v; returns the sub-vector of v starting with
      (v sub start) and ending with (v sub (finish - 1)).
      Returns the empty vector if (start = finish).
      Raise (Extract (start, finish)) if not (0 <= start,finish <= size v). *)


(* MANIPULATORS *)

  val rev: 'a Vector -> 'a Vector
   (* rev v; builds a vector containing the elements of v in reverse order. *)

  (* infix 6 ^ *)
  val ^ : 'a Vector * 'a Vector -> 'a Vector
   (* v ^ v'; builds a Vector containing the elements of v' appended to those
      of v. *)


(* REDUCERS *)

  val foldR: ('a -> 'b -> 'b) -> 'b -> 'a Vector -> 'b
   (* foldR f base v; folds using f associating to the right over the
      base element.
      foldR f base [a1,a2,...,an] = op(a1,f(a2,...,f(an,base)...)). *)

  val foldL: ('a -> 'b -> 'b) -> 'b -> 'a Vector -> 'b
   (* foldL f base v; folds using op associating to the left over the
      base element.
      foldL f base [a1,a2,...,an] = f(an,...,f(a2,f(a1,base))...). *)

  exception Empty of string

  val foldR': ('a -> 'a -> 'a) -> 'a Vector -> 'a
   (* foldR' f v; folds using f associating to the right over the
      last element of v.  Raises (Empty "foldR'") if v is empty. *)

  val foldL': ('a -> 'a -> 'a) -> 'a Vector -> 'a
   (* foldL' f v; folds using f associating to the left over the
      first element of v.  Raises (Empty "foldL'") if v is empty. *)

  val pairwise: ('a -> 'a -> bool) -> 'a Vector -> bool
   (* pairwise f v; true if (f (v sub i) (v sub (i + 1))) is true for all
      0 <= i < size v, or if v is empty. *)

(* ITERATORS *)

  val map: ('a -> 'b) -> 'a Vector -> 'b Vector
   (* map f v; builds a new vector by applying f to each element of v. *)

  val apply: ('a -> unit) -> 'a Vector -> unit
   (* apply f v; applies f to each element of v. *)

  val iterate: ('a * int -> 'b) -> 'a Vector -> 'b Vector
   (* iterate f v; builds a new vector by applying f to each element of v
      paired with its index. *)

  val iterateApply: ('a * int -> unit) -> 'a Vector -> unit
   (* iterateApply f v; applies f to each element of v paired
      with its index. *)
end
