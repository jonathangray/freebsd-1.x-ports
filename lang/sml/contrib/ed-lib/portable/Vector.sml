(*$Vector : VECTOR List *)

loadSig "VECTOR";

structure Vector: VECTOR  =

(* CONSTANT VECTORS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		4 Oct 1989

Maintenance:	Author


DESCRIPTION

   A constant vector is a unique sequence of objects that can't be changed.
   Vector is an equality type.


SEE ALSO

   VectorParse, Array.


RCS LOG

$Log: Vector.sml,v $
Revision 1.1  1994/02/08 00:23:21  jkh
Initial revision

Revision 1.10  91/03/06  16:38:39  16:38:39  db (Dave Berry)
Added print function(s).

Revision 1.9  91/02/22  19:09:31  19:09:31  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.8  91/02/11  20:55:30  20:55:30  db (Dave Berry)
Changed the name of this structure from Vector' to Vector.
Changed references to the List' structure to its new name: List.
Removed the comparison functions.
Added type synonym 'a T and the string and stringSep functions.
This forms part of the major reorganisation of the library.

Revision 1.7  91/01/30  18:40:40  18:40:40  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.6  91/01/28  13:01:28  13:01:28  db (Dave Berry)
Changed implementation to use CoreVector instead of List.

Revision 1.5  91/01/26  15:14:02  15:14:02  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.4  91/01/25  20:21:50  20:21:50  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/24  17:28:12  17:28:12  db (Dave Berry)
Removed version value.

Revision 1.2  91/01/15  10:59:29  10:59:29  db (Dave Berry)
Renamed "empty" function to "null", added "empty" constant.

Revision 1.1  90/12/20  15:53:58  15:53:58  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type 'a Vector = 'a CoreVector.vector
  type 'a T = 'a Vector


(* CONVERTORS *)

  fun list v =
	let fun list' i =
	      if i = CoreVector.length v then []
	      else CoreVector.sub (v, i) :: list' (i+1)
	in list' 0
	end

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

  fun map f v = CoreVector.vector (List.map f (list v))

  fun iterate f v = CoreVector.vector (List.iterate f (list v))

  fun apply f v = List.apply f (list v)

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
