(*$MonoList : MONO_LIST EQ_PRINT List *)

loadSig "MONO_LIST";

functor MonoList (
  structure Element: EQ_PRINT
): MONO_LIST =

(* MONOMORPHIC LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        15 Feb 1991

Maintenance:	Author 


DESCRIPTION

   A straightforward implementation; not always the most efficient.


RCS LOG

$Log: MonoList.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.3  91/03/06  16:38:25  16:38:25  db (Dave Berry)
Added print function(s).

Revision 1.2  91/02/22  19:09:22  19:09:22  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.1  91/02/15  17:11:42  17:11:42  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type Element = Element.T

  datatype MonoList = Mono of Element list

  type T = MonoList


(* CONSTANTS *)

  val empty = Mono []


(* CREATORS *)

  exception Size of string * int

  fun create n v =
        Mono (List.create n v)
	handle List.Size x => raise Size x

  fun tabulate (n, f) =
	Mono (List.tabulate (n, f))
	handle List.Size x => raise Size x

  fun tabulate' (n, f, base) =
	Mono (List.tabulate' (n, f, base))
	handle List.Size x => raise Size x


(* CONVERTORS *)


  fun stringSep start finish sep (Mono l) =
  	List.stringSep start finish sep Element.string l
 
  fun string l = stringSep ("[") ("]") (", ") l

  fun printSep os start finish sep (Mono l) =
  	List.printSep os start finish sep Element.print l
 
  fun print os l = printSep os ("[") ("]") (", ") l


(* OBSERVERS *)

  fun size (Mono l) = List.size l

  fun eq (Mono l) (Mono l') = List.eq Element.eq l l'

  val ne = not oo eq

  fun isEmpty l = (size l = 0)

  fun exists p (Mono l) = List.exists p l

  fun forAll p (Mono l) = List.forAll p l

  fun member a l = exists (Element.eq a) l

  fun index p (Mono l) = List.index p l


(* MANIPULATING THE LAST ELEMENT *)

  exception Empty of string

  fun last (Mono l) =
	List.last l

  fun dropLast (Mono l) =
	Mono (List.dropLast l)

  fun removeLast l = (last l, dropLast l)
      handle Empty _ => raise Empty "removeLast"

  fun changeLast f (Mono l) =
	Mono (List.changeLast f l)
	handle Empty _ => raise Empty "changeLast"

  fun updateLast v (Mono l) =
	Mono (List.updateLast v l)
	handle Empty _ => raise Empty "updateLast"

  fun insertLast (Mono l) (Mono l') =
	Mono (List.insertLast l l')
	handle Empty _ => raise Empty "insertLast"

  fun appendLast (Mono l) (Mono l') =
	Mono (List.appendLast l l')
	handle Empty _ => raise Empty "appendLast"

  fun spliceLast (Mono l) (Mono l') =
	Mono (List.spliceLast l l')
	handle Empty _ => raise Empty "spliceLast"


(* MANIPULATING THE NTH ELEMENT *)

  infix 9 sub
  exception Subscript of string * int

  fun (Mono l) sub n =
	List.sub (l, n)
	handle List.Subscript x => raise Subscript x

  fun nth n l =
	l sub n
	handle Subscript _ => raise Subscript ("nth", n)

  fun dropNth n (Mono l) =
	Mono (List.dropNth n l)
	handle List.Subscript x => raise Subscript x

  fun removeNth n l =
	(nth n l, dropNth n l)
	handle List.Subscript _ => raise Subscript ("removeNth", n)

  fun splitNth n (Mono l) =
	let val (l1, l2) = List.splitNth n l
	in (Mono l1, Mono l2)
	end
	handle List.Subscript x => raise Subscript x

  fun changeNth n f (Mono l) =
	Mono (List.changeNth n f l)
	handle List.Subscript x => raise Subscript x

  fun updateNth n v (Mono l) =
	Mono (List.updateNth n v l)
	handle List.Subscript x => raise Subscript x

  fun insertNth n (Mono l) (Mono l') =
	Mono (List.insertNth n l l')
	handle List.Subscript x => raise Subscript x

  fun appendNth n (Mono l) (Mono l') =
	Mono (List.appendNth n l l')
	handle List.Subscript x => raise Subscript x

  fun spliceNth n (Mono l) (Mono l') =
	Mono (List.spliceNth n l l')
	handle List.Subscript x => raise Subscript x


(* ACCESSING A RANGE OF ELEMENTS *)

  exception ExtractLast of int
  fun extractLast start (Mono l) =
	Mono (List.extractLast start l)
	handle List.ExtractLast _ => raise ExtractLast start

  exception Extract of int * int
  fun extract start finish (Mono l) =
	Mono (List.extract start finish l)
	handle List.Extract _ => raise Extract (start, finish)


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  fun first p (Mono l) =
	List.first p l
	handle List.First _ => raise First "first"

  fun dropFirst p (Mono l) =
	Mono (List.dropFirst p l)
	handle List.First _ => raise First "dropFirst"

  fun removeFirst p l =
	(first p l, dropFirst p l)
	handle First _ => raise First "removeFirst"

  fun splitFirst p (Mono l) =
	let val (l1, l2) = List.splitFirst p l
	in (Mono l1, Mono l2)
	end
	handle List.First _ => raise First "splitFirst"

  fun changeFirst p f (Mono l) =
	Mono (List.changeFirst p f l)
	handle List.First _ => raise First "changeFirst"

  fun updateFirst p a (Mono l) =
	Mono (List.updateFirst p a l)
	handle List.First _ => raise First "updateFirst"

  fun insertFirst p (Mono l) (Mono l') =
	Mono (List.insertFirst p l l')
	handle List.First _ => raise First "insertFirst"

  fun appendFirst p (Mono l) (Mono l') =
	Mono (List.appendFirst p l l')
	handle List.First _ => raise First "appendFirst"

  fun spliceFirst p (Mono l) (Mono l') =
	Mono (List.spliceFirst p l l')
	handle List.First _ => raise First "spliceFirst"


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  fun prefix p (Mono l) = Mono (List.prefix p l)

  fun dropPrefix p (Mono l) = Mono (List.dropPrefix p l)

  fun removePrefix p l = (prefix p l, dropPrefix p l)


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  fun all p (Mono l) = Mono (List.all p l)

  fun dropAll p = all (not o p);

  fun removeAll p l = (all p l, dropAll p l)

  fun updateAll p v (Mono l) = Mono (List.updateAll p v l)

  fun changeAll p f (Mono l) = Mono (List.changeAll p f l)

  fun insertAll p (Mono l) (Mono l') = Mono (List.insertAll p l l')

  fun appendAll p (Mono l) (Mono l') = Mono (List.appendAll p l l')

  fun spliceAll p (Mono l) (Mono l') = Mono (List.spliceAll p l l')


(* REDUCERS *)

  fun foldR f base (Mono l) = List.foldR f base l

  fun foldL f base (Mono l) = List.foldL f base l

  fun foldR' f (Mono []) = raise Empty "foldR'"
  |   foldR' f (Mono l) = List.foldR' f l

  fun foldL' f (Mono []) = raise Empty "foldL'"
  |   foldL' f (Mono l) = List.foldL' f l

  fun pairwise f (Mono l) = List.pairwise f l


(* OTHER MANIPULATORS *)

  fun appendIfAll p (Mono l) (Mono l') = Mono (List.appendIfAll p l l')
  
  fun rev (Mono l) = Mono (List.rev l)

  infixr 5 @
  fun (Mono l) @ (Mono l') = Mono (List.@ (l, l'))


(* ITERATORS *)

  fun map f (Mono l) = Mono (List.map f l)

  fun mapAll p f (Mono l) = Mono (List.mapAll p f l)

  fun iterate f (Mono l) = Mono (List.iterate f l)

  fun apply f (Mono l) = List.apply f l

  fun applyAll p f (Mono l) = List.applyAll p f l

  fun iterateApply f (Mono l) = List.iterateApply f l

end
