(*$MONO_LIST: GeneralTypes *)

signature MONO_LIST =
sig

(* MONOMORPHIC LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		15 Feb 1991

Maintenance:	Author


DESCRIPTION

   Monomorphic lists.

   Many of these functions use the orthogonal naming scheme described in the
   LIST signature.  They can be grouped as follows:

   f;
     if f is "last", return the last element.
     if f is "nth", return the nth element (counting from zero).
     if f is "first", return the first element that satisfies a predicate p.
     if f is "all", return the list of elements that satisfy p.
     if f is "prefix", return the prefix of elements that satisfy p.

   dropF ... l; l with (f l) removed.
   splitF ... l; a pair of lists:
		 (elements before f l, elements from f l to last l (inc.)).
   removeF ... l; a pair of (f l, dropF l).
   updateF ... l; l with (f l) replaced with a value.
   changeF ... l; l with (f l) replaced with (g (f l)).
   spliceF ... l; l with (f l) replaced with the elements in another list.
   insertF ... l; l with the elements in another list inserted before (f l).
   appendF ... l; l with the elements in another list inserted after (f l).

   splitLast, splitAll, and splitPrefix are not provided because "all" and
   "prefix" return lists and because splitLast would be the same as removeLast.

   changePrefix, updatePrefix, insertPrefix, appendPrefix, and splicePrefix
   are not provided, because "prefix" returns a list.
   
   The "last" functions raise "Empty" if l is empty.
   The "nth" functions raise "Subscript" if not (0 <= n < size l).
   The "first" functions (except dropFirst) raise "First" if there aren't
	any elements that satisfy p.


SEE ALSO

   LIST, MONO_VECTOR, MONO_ARRAY.


NOTES

   Possibly there should be a dropExtract (aka delete?) function.


RCS LOG

$Log: MONO_LIST.sml,v $
Revision 1.1  1994/02/08 00:23:27  jkh
Initial revision

Revision 1.3  91/03/06  16:29:08  16:29:08  db (Dave Berry)
Added print function(s).

Revision 1.2  91/02/22  19:03:18  19:03:18  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.1  91/02/15  17:11:22  17:11:22  db (Dave Berry)
Initial revision



*)


(* TYPES *)

  type MonoList
  type Element
  type T
    sharing type T = MonoList


(* CONSTANTS *)

  val empty: MonoList
   (* empty; the empty list. *)


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the creation functions when they are invoked
      with a negative size. *)

  val create: Nat -> Element -> MonoList
   (* create n e; create a list of size n, each element being e. *)

  val tabulate: Nat * (int -> Element) * MonoList
  (* tabulate (n, f); create a list of size n, sych that the element
     with index i is initialised to (f i). *)

  val tabulate': Nat * ('b -> Element * 'b) * 'b -> MonoList
  (* tabulate' (n, f, base); create a list l of size n, with (l sub 0)
     initialised to (# 1 (f base)) and (l sub i (i > 0)) initialised to
     (# 1 (f (# 2 f_i))), where f_i is the result of the i^th application
     of f. *)


(* CONVERTORS *)

  val stringSep: string -> string -> string -> MonoList -> string
   (* stringSep start finish sep l; returns the string representation of l,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: MonoList -> string
   (* string l; returns the canonical string representation of l. *)

  val printSep: outstream -> string -> string -> string ->
                (outstream -> 'a -> unit) -> MonoList -> unit
   (* printSep os start finish sep p l; sends the string representation of l
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: outstream -> (outstream -> 'a -> unit) -> MonoList -> unit
   (* print os p l; sends the canonical string representation of l to
      the stream os. *)


(* OBSERVERS *)

  val size: MonoList -> Nat
   (* size l; returns the number of elements in l. *)

  val isEmpty: MonoList -> bool
   (* isEmpty l; returns true if l is the empty list. *)

  val exists: (Element -> bool) -> MonoList -> bool
   (* exists p l; true if there exists an element of l satisfying p. *)

  val forAll: (Element -> bool) -> MonoList -> bool
   (* forAll p l; true if every element of l satisfies p. *)

  val member: Element -> MonoList -> bool
   (* member a l; true if a is an element of l. *)

  val eq: MonoList -> MonoList -> bool
   (* eq x y; returns true if (size x = size y) and for all i,
      0 <= i < size x, (p (x sub i) (y sub i)). *)

  val ne: MonoList -> MonoList -> bool
   (* ne x y; returns true if (size x <> size y) or there exists
      an i such that 0 <= i < size x and (p (x sub i) (y sub i)). *)

  val index: (Element -> bool) -> MonoList -> (int, unit) Result
   (* index p l; returns the position in l of the first element
      of l satisfying p. *)


(* MANIPULATING THE LAST ELEMENT *)

  exception Empty of string

  val last: MonoList -> Element
   (* last l; returns the last element of l.  Raises (Empty "last") if l
      is empty. *)

  val dropLast: MonoList -> MonoList
   (* dropLast l; returns l without its last element.
      Raises (Empty "dropLast") if l is empty. *)

  val removeLast: MonoList -> (Element * MonoList)
   (* removeLast l = (lastl, dropLast l).
      Raises (Empty "removeLast") if l is empty. *)

  val updateLast: Element -> MonoList -> MonoList
   (* updateLast v l; returns l with its last element replaced by v.
      Raises (Empty "updateLast") if l is empty. *)

  val changeLast: (Element -> Element) -> MonoList -> MonoList
   (* changeLast f l; returns l with (last l) replaced by (f (last l)).
      Raises (Empty "changeLast") if l is empty. *)

  val insertLast: MonoList -> MonoList -> MonoList
   (* insertLast l' l; returns l with the elements of l' inserted before
      (last l).  Raises (Empty "insertLast") if l is empty. *)

  val appendLast: MonoList -> MonoList -> MonoList
   (* appendLast l' l; returns l with the elements of l' appended after
      (last l).  Raises (Empty "insertLast") if l is empty. *)

  val spliceLast: MonoList -> MonoList -> MonoList
   (* spliceLast l' l; returns l with (last l) replaced by the elements of l'.
      Raises (Empty "spliceLast") if l is empty. *)


(* MANIPULATING THE NTH ELEMENT *)

  exception Subscript of string * int

  (* infix 9 sub *)
  val sub: MonoList * int -> Element
   (* l sub n; returns the n-1'th element of l.
      Raises (Subscript ("sub", n)) if not (0 <= n < size l). *)

  val nth: int -> MonoList -> Element
   (* nth n l; returns the n-1'th element of l.
      Raises (Subscript ("nth", n)) if not (0 <= n < size l). *)

  val removeNth: int -> MonoList -> (Element * MonoList)
   (* removeNth n l= (l sub n, dropNth n l).
      Raises (Subscript ("removeNth", n)) if not (0 <= n < size l). *)

  val splitNth: int -> MonoList -> (MonoList * MonoList)
   (* splitNth n l; returns ([l sub 0, ... l sub (n-1)], [l sub n, ... last l]).
      Raises (Subscript ("splitNth", n)) if not (0 <= n <= size l - 1). *)

  val dropNth: int -> MonoList -> MonoList
   (* dropNth n l; returns l without (l sub n).
      Raises (Subscript ("dropNth", n)) if not (0 <= n < size l). *)

  val updateNth: int -> Element -> MonoList -> MonoList
   (* updateNth n v l; returns l with (l sub n) replaced by v.
      Raises (Subscript ("updateNth", n)) if not (0 <= n < size l). *)

  val changeNth: int -> (Element -> Element) -> MonoList -> MonoList
   (* changeNth n f l; returns l with (l sub n) replaced by (f (l sub n)).
      Raises (Subscript ("changeNth", n)) if not (0 <= n < size l). *)

  val insertNth: int -> MonoList -> MonoList -> MonoList
   (* insertNth n l' l; returns l with the elements of l' inserted before
      (l sub n).
      Raises (Subscript ("insertNth", n)) if not (0 <= n < size l). *)

  val appendNth: int -> MonoList -> MonoList -> MonoList
   (* appendNth n l' l; returns l with the elements of l' appended after
      (l sub n).
      Raises (Subscript ("insertNth", n)) if not (0 <= n < size l). *)

  val spliceNth: int -> MonoList -> MonoList -> MonoList
   (* spliceNth n l' l; returns l with (l sub n) replaced by the elements of
      l'.  Raises (Subscript ("spliceNth", n)) if not (0 <= n < size l). *)


(* ACCESSING A RANGE OF ELEMENTS *)

  exception ExtractLast of int
  val extractLast: int -> MonoList -> MonoList
   (* extractLast l start ; returns the elements of l from (l sub start) to
      last l.  Raises (ExtractLast start) if not (0 <= start < size l). *)

  exception Extract of int * int
  val extract: int -> int -> MonoList -> MonoList
   (* extract start finish l; returns the elements of l from (l sub start) to
      (l sub (finish - 1)).  Returns [] if (start = finish).  Raises
      (Extract (start, finish)) if not (0 <= start <= finish <= size l). *)


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  val first: (Element -> bool) -> MonoList -> Element
   (* first p l; returns the first element in l satisfying p.
      Raises (First "first") if p doesn't hold for any element of l. *)

  val dropFirst: (Element -> bool) -> MonoList -> MonoList
   (* dropFirst p l; returns l without the first of its elements (if any)
      that satisfy p. *)

  val removeFirst: (Element -> bool) -> MonoList -> (Element * MonoList)
   (* removeFirst p l = (first l, dropFirst l).
      Raises (First "removeFirst") if p doesn't hold for any element of l. *)

  val splitFirst: (Element -> bool) -> MonoList -> (MonoList * MonoList)
   (* splitFirst p l; returns (extract 0 n l, extractLast n l),
      where (l sub n) is the first element of l that satisfies p.
      Raises (First "splitFirst") if p doesn't hold for any element of l. *)

  val updateFirst: (Element -> bool) -> Element -> MonoList -> MonoList
   (* updateFirst p v l; returns l with (first p l) replaced by v.
      Raises (First "updateFirst") if there is no (first p l). *)

  val changeFirst: (Element -> bool) -> (Element -> Element) ->
		   MonoList -> MonoList
   (* changeFirst p f l; returns l with (first p l) replaced by
      (f (first p l)).  Raises (First "changeFirst") if there is no
      (first p l). *)

  val insertFirst: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* insertFirst p l' l; returns l with the elements of l' inserted before
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val appendFirst: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* appendFirst p l' l; returns l with the elements of l' appended after
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val spliceFirst: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* spliceFirst p l' l; returns l with (first p l) replaced by the elements
      of l'.  Raises (First "spliceFirst") if there is no (first p l). *)


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  val prefix: (Element -> bool) -> MonoList -> MonoList
   (* prefix p l; returns the largest prefix of l each of whose
      elements satisfies p *)

  val dropPrefix: (Element -> bool) -> MonoList -> MonoList
   (* dropPrefix p l; returns l without the largest prefix of l
      each of whose elements satisfies p *)

  val removePrefix: (Element -> bool) -> MonoList -> (MonoList * MonoList)
   (* removePrefix p l = (prefix p l, dropPrefix p l). *)


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  val all: (Element -> bool) -> MonoList -> MonoList
   (* all p l: returns a list of the elements of l that satisfy p. *)

  val dropAll: (Element -> bool) -> MonoList -> MonoList
   (* dropAll p l: returns a list of the elements of l that don't satisfy p. *)

  val removeAll: (Element -> bool) -> MonoList -> (MonoList * MonoList)
   (* removeAll p l = (all p l, dropAll p l). *)

  val updateAll: (Element -> bool) -> Element -> MonoList -> MonoList
   (* updateAll p v l; returns l with each element that satisfies p
      replaced by v. *)

  val changeAll: (Element -> bool) -> (Element -> Element) ->
		 MonoList -> MonoList
   (* changeAll p f l; returns l with each element e that satisfies p
      replaced by (f e). *)

  val insertAll: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* insertAll p l' l; returns l with the elements of l' inserted before each
      element of l that satisfies p. *)

  val appendAll: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* appendAll p l' l; returns l with the elements of l' appended after each
      element of l that satisfies p. *)

  val spliceAll: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* spliceAll p l' l; returns l with each element that satisfies p
      replaced by the elements of l'. *)


(* OTHER MANIPULATORS *)

  val appendIfAll: (Element -> bool) -> MonoList -> MonoList -> MonoList
   (* appendIfAll p l' l; appends l' at the end of l if every element of
      l satisfies p. *)

  val rev: MonoList -> MonoList
   (* rev l; builds a new MonoList containing the elements of l in reverse
      order. *)

  (* infixr 5 @ *)
  val @ : MonoList * MonoList -> MonoList
   (* l @ l'; builds a new MonoVector containing the elements of l' appended
      to those of l. *)


(* ITERATORS *)

  val map: (Element -> Element) -> MonoList -> MonoList
   (* map f l; builds a new MonoList by applying f to each element of l. *)

  val mapAll: (Element -> bool) -> (Element -> Element) -> MonoList -> MonoList
   (* mapAll p f l; builds a new MonoList by applying f to each element
      of l that satisfies p. *)

  val iterate: (Element * int -> Element) -> MonoList -> MonoList
   (* iterate f l; builds a new MonoList by applying f to each element
      of l paired with its index. *)

  val apply: (Element -> unit) -> MonoList -> unit
   (* apply f l; applies f to each element of l. *)

  val applyAll: (Element -> bool) -> (Element -> unit) -> MonoList -> unit
   (* applyAll p f l; applies f to each element of l that satisfies p. *)

  val iterateApply: (Element * int -> unit) -> MonoList -> unit
   (* iterateApply f l; applies f to each element of l paired
      with its index. *)


(* REDUCERS *)

  val foldR: (Element -> 'b -> 'b) -> 'b -> MonoList -> 'b
   (* foldR f base l; folds using f associating to the right over the
      base element.
      foldR f [a1,a2,...,an] base = f a1 (f a2 ... (op an base)...). *)

  val foldL: (Element -> 'b -> 'b) -> 'b -> MonoList -> 'b
   (* foldL f l base; folds using f associating to the left over the
      base element.
      foldL f [a1,a2,...,an] base = f an ... (f a2 (f a1 base))... . *)

  val foldR': (Element -> Element -> Element) -> MonoList -> Element
   (* foldR' f l; folds using f associating to the right over the
      last element of l.  Raises (Empty "foldR'") if l is empty. *)

  val foldL': (Element -> Element -> Element) -> MonoList -> Element
   (* foldL' f l; folds using f associating to the left over the
      first element of l.  Raises (Empty "foldL'") if l is empty. *)

  val pairwise: (Element -> Element -> bool) -> MonoList -> bool
   (* pairwise f l; true if (f (l sub i) (l sub (i + 1))) is true for all
      0 <= i < size l, or if l is empty. *)
end

