(*$MonoVector : MONO_VECTOR EQ_PRINT Vector *)

loadSig "MONO_VECTOR";

functor MonoVector (
  structure Element: EQ_PRINT
): MONO_VECTOR =

(* CONSTANT VECTORS OF A GIVEN TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		21 Dec 1989

Maintenance:	Author


DESCRIPTION

   A constant vector is a unique sequence of objects that can't be changed.
   Vector is an equality type.  There is only one empty Vector.

   This is a reference implementation.  Most systems will provide most of
   the following as built-in functions.  Any sensible implementation will
   provide constant time access to each element.


SEE ALSO

   Vector, MonoArray.


RCS LOG

$Log: MonoVector.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.13  91/03/06  16:38:28  16:38:28  db (Dave Berry)
Added print function(s).

Revision 1.12  91/02/22  19:09:33  19:09:33  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.11  91/02/12  18:15:36  18:15:36  db (Dave Berry)
Changed the definition of ne to use the oo compose function, which is
now avaiable at top-level.

Revision 1.10  91/02/11  20:28:19  20:28:19  db (Dave Berry)
Renamed Object to Element since it isn't an OBJECT anymore.
Moved the read and parse functions to MonoVectorParse.sml.
Removed the sort and comparison functions.
This forms part of the major reorganisation of the library.

Revision 1.9  91/02/01  11:38:13  11:38:13  db (Dave Berry)
Fixed two calls of Vector.null that should have been changed to
Vector.isEmpty.

Revision 1.8  91/01/30  18:40:38  18:40:38  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.7  91/01/30  17:43:01  17:43:01  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.6  91/01/26  15:13:58  15:13:58  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.5  91/01/25  20:18:35  20:18:35  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.4  91/01/24  17:25:14  17:25:14  db (Dave Berry)
Removed version value.

Revision 1.3  91/01/15  11:00:47  11:00:47  db (Dave Berry)
Renamed "empty" function to "null", added "empty" constant.
Changed type to datatype, so that there is one empty vector for each
type (i.e. each application of the function creates a new empty vector).

Revision 1.2  91/01/10  15:58:52  15:58:52  db (Dave Berry)
List.ge and List.le now take le and ge as arguments instead of
lt and gt.

Revision 1.1  90/12/20  15:01:54  15:01:54  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type Element = Element.T

  datatype MonoVector = Mono of Element Vector.Vector

  type T = MonoVector


(* CONSTANTS *)

  val empty = Mono Vector.empty


(* CREATORS *)

  exception Size of string * int

  fun create size init =
	Mono (Vector.create size init)
	handle Vector.Size x => raise Size x

  fun tabulate (size, f) =
	Mono (Vector.tabulate (size, f))
	handle Vector.Size x => raise Size x

  fun tabulate' (size, f, base) =
	Mono (Vector.tabulate' (size, f, base))
	handle Vector.Size x => raise Size x


(* ITERATORS *)

  fun map f (Mono v) = Mono (Vector.map f v)

  fun apply f (Mono v) = Vector.apply f v

  fun iterate f (Mono v) = Mono (Vector.iterate f v)

  fun iterateApply f (Mono v) = Vector.iterateApply f v


(* OBSERVERS *)

  fun size (Mono v) = Vector.size v

  fun isEmpty v = (size v = 0)

  fun eq (Mono v) (Mono v') = Vector.eq Element.eq v v'

  val ne = not oo eq


(* CONVERTORS *)

  fun list (Mono v) = Vector.list v

  val fromList = Mono o Vector.fromList

  fun stringSep start finish sep (Mono v) =
        Vector.stringSep start finish sep Element.string v

  fun string v =
	if Element.fixedWidth
	then stringSep "" "" "" v
	else stringSep "" "" " " v

  fun printSep os start finish sep (Mono v) =
        Vector.printSep os start finish sep Element.print v

  fun print os v =
	if Element.fixedWidth
	then printSep os "" "" "" v
	else printSep os "" "" " " v


(* SELECTORS *)

  exception Subscript of string * int
  fun sub (Mono v, n) = Vector.sub (v, n)
	    handle Vector.Subscript x => raise Subscript x
  infix 9 sub;

  fun nth n v = v sub n
		handle Subscript _ => raise Subscript ("nth", n)

  exception Extract of int * int
  fun extract start finish (Mono v) =
	Mono (Vector.extract start finish v)
	handle Vector.Extract x => raise Extract x


(* MANIPULATORS *)

  fun rev (Mono v) = Mono (Vector.rev v)

  infix 6 ^
  fun (Mono v) ^ (Mono v') = Mono (Vector.^ (v, v'))


(* REDUCERS *)

  fun foldL f b (Mono v) = Vector.foldL f b v

  fun foldR f b (Mono v) = Vector.foldR f b v

  exception Empty of string

  fun foldR' f (Mono v) =
	if Vector.isEmpty v then raise Empty "foldR'"
        else Vector.foldR' f v

  fun foldL' f (Mono v) =
	if Vector.isEmpty v then raise Empty "foldL'"
        else Vector.foldL' f v

  fun pairwise f (Mono v) = Vector.pairwise f v
end
