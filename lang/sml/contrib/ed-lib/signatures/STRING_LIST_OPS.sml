(*$STRING_LIST_OPS *)

signature STRING_LIST_OPS =
sig

(* LIST-LIKE OPERATIONS ON ASCII STRINGS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        10 Feb 1991

Maintenance:	Author


DESCRIPTION

   Functions on strings that parallel the functions defined on lists by
   the LIST signature.  They use an orthogonal naming scheme.  They can
   be grouped as follows:

   f;
     if f is "nth", return the nth character (counting from zero).
     if f is "first", return the first character that satisfies a predicate p.
     if f is "all", return the list of characters that satisfy p.
     if f is "prefix", return the prefix of characters that satisfy p.

   dropF ... s; s with (f s) removed.
   splitF ... s; a pair of strings:
		 (characters before f s, characters from f s to the end of s).
   removeF ... s; a pair of (f s, dropF s).
   updateF ... s; l with (f s) replaced with a value.
   changeF ... s; l with (f s) replaced with (g (f s)).
   spliceF ... s; l with (f s) replaced with the elements in another string.
   insertF ... s; l with another string inserted before (f s).
   appendF ... s; l with another string inserted after (f s).

   splitAll, and splitPrefix are not provided because "all" and
   "prefix" return strings of arbitrary length.

   changePrefix, updatePrefix, insertPrefix, appendPrefix, and splicePrefix
   are not provided, because "prefix" returns a string of arbitrary length.

   The "nth" functions raise "Subscript" if not (0 <= n < size s).
   The "first" functions (except dropFirst) raise "First" if there aren't
        any elements that satisfy p.


NOTES

   Possibly should add revFirst, dropRevFirst, ... and postfix, dropPostfix,
   ... sets of functions.

   Possibly first, ... and prefix, ... should take an integer offset.


SEE ALSO

   STRING, LIST.


NOTES

   These functions used to be in the main STRING signature.
   A few of these functions are duplicates of functions in the
   main STRING signature.


RCS LOG

$Log: STRING_LIST_OPS.sml,v $
Revision 1.1  1994/02/08 00:23:27  jkh
Initial revision

Revision 1.2  91/02/22  19:04:39  19:04:39  db (Dave Berry)
Renamed Sub to Subscript, to match existing convention.

Revision 1.1  91/02/11  19:30:34  19:30:34  db (Dave Berry)
Initial revision


*)


(* MANIPULATING THE NTH ELEMENT *)

  exception Subscript of string * int
  (* Subscript (fn, n); raised if the function named fn is called with
     an out of range argument n. *)

  (* infix 9 sub *)
  val sub: string * int -> string
   (* s sub n; return the nth character of s.
      Raises (Subscript ("sub", n)) if not (0 <= n < size s). *)

  val nth: int -> string -> string
   (* nth n s; return the nth character of s.
      Raises (Subscript ("nth", n)) if not (0 <= n < size s). *)

  val dropNth: int -> string -> string
   (* dropNth n s = returns s without (s sub n).
      Raises (Subscript ("dropNth", n)) if not (0 <= n < size s). *)

  val removeNth: int -> string -> (string * string)
   (* removeNth n s = (s sub n, dropNth n s).
      Raises (Subscript ("removeNth", n)) if not (0 <= n < size s). *)

  val splitNth: int -> string -> (string * string)
   (* splitNth n s; returns (extract 0 n s, extract n (size s) s).
      Raises (Subscript ("splitNth", n)) if not (0 <= n < size s). *)

  exception Char of string * string
   (* Char (fn, c); raised if the function called fn is called with a
      string c such that (size c <> 1).  The idea is that the update functions
      replace one character with another, rather than with an arbitrary length
      string.  *)

  val updateNth: int -> string -> string -> string
   (* updateNth n s' s; returns s with (s sub n) replaced by s'.
      Raises (Char ("updateNth", s')) if (size s' <> 1).
      Raises (Subscript ("updateNth", n)) if not (0 <= n < size s). *)

  val changeNth: int -> (string -> string) -> string -> string
   (* changeNth n f s; returns s with (s sub n) replaced by (f (s sub n)).
      Raises (Subscript ("changeNth", n)) if not (0 <= n < size s). *)

  val insertNth: int -> string -> string -> string
   (* insertNth n s' s; returns s with s' inserted before (s sub n).
      Raises (Subscript ("insertNth", n)) if not (0 <= n < size s). *)

  val appendNth: int -> string -> string -> string
   (* appendNth n s' s; returns s with s' appended after (s sub n).
      Raises (Subscript ("appendNth", n)) if not (0 <= n < size s). *)

  val spliceNth: int -> string -> string -> string
   (* spliceNth n s' s; returns s with (s sub n) replaced by s'.
      Unlike updateNth, s' may be any length.
      Raises (Subscript ("spliceNth", n)) if not (0 <= n < size s). *)


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  val first: (string -> bool) -> string -> string
   (* first p s; returns the first character in s satisfying p.
      Raises (First "first") if p doesn't hold for any character in s. *)

  val dropFirst: (string -> bool) -> string -> string 
   (* dropFirst p s; returns s without the first of its characters (if any)
      that satisfy p. *)

  val removeFirst: (string -> bool) -> string -> (string * string)
   (* removeFirst p s = (first s, dropFirst s).
      Raises (First "removeFirst") if p doesn't hold for any character in s. *)

  val splitFirst: (string -> bool) -> string -> (string * string)
   (* splitFirst p s; returns (extract 0 n s, extract n (size s - 1) s),
      where (s sub n) is the first character in s that satisfies p.
      Raises (First "splitFirst") if p doesn't hold for any character in s. *)

  val updateFirst: (string -> bool) -> string -> string -> string
   (* updateFirst p s' s; returns s with (first p s) replaced by s'.
      Raises (First "updateFirst") if there is no (first p s).
      Raises (Char ("updateFirst", s')) if (size s' <> 1). *)

  val changeFirst: (string -> bool) -> (string -> string) -> string -> string
   (* changeFirst p f s; returns s with (first p s) replaced by
      (f (first p s)).  Raises (First "changeFirst") if there is no
      (first p s).  *)

  val insertFirst: (string -> bool) -> string -> string -> string
   (* insertFirst p s' s; returns s with s' inserted before
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val appendFirst: (string -> bool) -> string -> string -> string
   (* appendFirst p s' s; returns s with s' appended after
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val spliceFirst: (string -> bool) -> string -> string -> string
   (* spliceFirst p s' s; returns s with (first p s) replaced by s'.
      Raises (First "spliceFirst") if there is no (first p s).
      Unlike updateFirst, s' may be any length. *)


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  val prefix: (string -> bool) -> string -> string
   (* getPrefix p s; returns the largest prefix of s in which each character
      satisfies p. *)

  val dropPrefix: (string -> bool) -> string -> string
   (* dropPrefix p s; returns s without the largest prefix in which
      every character satisfies p. *)

  val removePrefix: (string -> bool) -> string -> string * string
   (* removePrefix p s = (prefix p s, dropPrefix p s). *)


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  val all: (string -> bool) -> string -> string
   (* all p s: returns a string formed from the characters in s that
      satisfy p. *)

  val dropAll: (string -> bool) -> string -> string
   (* dropAll p s: returns a string formed from the characters in s that don't
      satisfy p. *)

  val removeAll: (string -> bool) -> string -> (string * string)
   (* removeAll p s = (all p s, dropAll p s). *)

  val updateAll: (string -> bool) -> string -> string -> string
   (* updateAll p s' s; returns s with each character that satisfies p
      replaced by s'.  Raises (Char ("updateAll", s')) if (size s' <> 1). *)

  val changeAll: (string -> bool) -> (string -> string) -> string -> string
   (* changeAll p f s; returns s with each character c that satisfies p
      replaced by (f c). *)

  val insertAll: (string -> bool) -> string -> string -> string
   (* insertAll p s' s; returns s with s' inserted before each
      character in s that satisfies p. *)

  val appendAll: (string -> bool) -> string -> string -> string
   (* appendAll p s' s; returns s with s' appended after each
      character in s that satisfies p. *)

  val spliceAll: (string -> bool) -> string -> string -> string
   (* spliceAll p s' s; returns s with each character that satisfies p
      replaced by s'. *)

end
