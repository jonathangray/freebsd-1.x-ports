(*$Vector : VECTOR List *)

loadSig "VECTOR";

structure Vector: VECTOR  =

(* CONSTANT VECTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		4 Oct 1989

Maintenance:	Author


DESCRIPTION

   Poplog ML provides the list, map, apply and iterate functions.


SEE ALSO

   VectorParse, Array.


RCS LOG

$Log: Vector.sml,v $
Revision 1.1  1994/02/08 00:23:16  jkh
Initial revision

Revision 1.1  91/04/10  16:59:43  16:59:43  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type 'a Vector = 'a CoreVector.vector
  type 'a T = 'a Vector


(* CONVERTORS *)

  val list = ExtendedVector.to_list

  val fromList = CoreVector.vector

  fun stringSep start finish sep p v =
	List.stringSep start finish sep p (list v)

  fun string p v = stringSep "" "" " " p v

  fun printSep os start finish sep p v =
	List.printSep os start finish sep p (list v)

  fun print os p v = printSep os "" "" " " p v


(* CONSTANTS *)

  val empty = CoreVector.vector []


(* CREATORS *)

  exception Size of string * int

  fun create size init =
	if size < 0 then raise Size ("create", size)
	else CoreVector.vector (List.create size init)

  fun tabulate (size, f) =
	CoreVector.tabulate (size, f)
	handle CoreVector.Size => raise Size ("tabulate", size)

  fun tabulate' (size, f, base) =
	if size < 0 then raise Size ("tabulate'", size)
	else CoreVector.vector (List.tabulate' (size, f, base))


(* ITERATORS *)

  val map = ExtendedVector.map

  val iterate = ExtendedVector.iterate

  val apply = ExtendedVector.app

  fun iterateApply f v = List.iterateApply f (list v)


(* OBSERVERS *)

  val size = CoreVector.length

  fun isEmpty v = (size v = 0)

  fun eq p v v' =
	List.eq p (list v) (list v')

  fun ne p v v' =
	List.ne p (list v) (list v')


(* MANIPULATORS *)

  fun rev v = CoreVector.vector (List.rev (list v))

  infix 6 ^
  fun op ^ (v, v') =
	CoreVector.vector ((list v) @ (list v'))


(* SELECTORS *)

  exception Subscript of string * int

  infix 9 sub;
  fun op sub (v, i) =
	CoreVector.sub (v, i)
	handle CoreVector.Subscript => raise Subscript ("sub", i)

  fun nth n v = v sub n
		handle Subscript _ => raise Subscript ("nth", n)

  exception Extract of int * int
  fun extract start finish v =
	CoreVector.vector (List.extract start finish (list v))
	handle List.Extract x => raise Extract x


(* REDUCERS *)

  fun foldL f base v = List.foldL f base (list v)

  fun foldR f base v = List.foldR f base (list v)

  exception Empty of string

  fun foldR' f v = if size v = 0 then raise Empty "foldR'"
  		   else List.foldR'  f (list v)

  fun foldL' f v = if size v = 0 then raise Empty "foldL'"
  		   else List.foldL' f (list v)

  fun pairwise f v = List.pairwise f (list v)
end
