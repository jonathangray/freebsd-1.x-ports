(*$MonoArray : MONO_ARRAY EQ_PRINT Array *)

loadSig "MONO_ARRAY";

functor MonoArray (
  structure Element: EQ_PRINT
): MONO_ARRAY =

(* ARRAYS OF A GIVEN TYPE

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		12 Feb 1990

Maintenance:	Author


DESCRIPTION

   A MonoArray is a unique sequence of objects that can be updated in place.
   Each application of the functor generates exactly one empty Array.

   This is a reference implementation.  Most systems will provide most of
   the following as built-in functions.  Any sensible implementation will
   provide constant time access to each element.


SEE ALSO

   MonoVector, Array.


RCS LOG

$Log: MonoArray.sml,v $
Revision 1.1  1994/02/08 00:23:19  jkh
Initial revision

Revision 1.14  91/03/06  16:38:22  16:38:22  db (Dave Berry)
Added print function(s).

Revision 1.13  91/02/22  19:08:44  19:08:44  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.
Uncurried update to match standard agreed for arrays and vectors by the SML
  implementers, and changed the order of arguments of updateRange and copy
  to match (although these functions are still curried).

Revision 1.12  91/02/12  19:38:34  19:38:34  db (Dave Berry)
Made MonoArrays be a reference to an Array; now they can be tested
for identity.  Made eq and ne test for identity.  Added Null MonoArray
so that all empty MonoArrays resulting from the same application of
the functor are identical.

Revision 1.11  91/02/12  18:15:31  18:15:31  db (Dave Berry)
The GENERAL_TYPES signature now includes the infix functions defined in
General.sml, so this entry makes them infix at top-level.

Revision 1.10  91/02/11  20:25:30  20:25:30  db (Dave Berry)
Renamed Object to Element since it isn't an OBJECT anymore.
Moved the read and parse functions to MonoArrayParse.sml.
Removed the sort and comparison functions.
This forms part of the major reorganisation of the library.

Revision 1.9  91/02/01  11:39:47  11:39:47  db (Dave Berry)
Fixed two calls of Array.null that should have been changed to
Array.isEmpty.

Revision 1.8  91/01/30  18:40:35  18:40:35  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.7  91/01/30  17:42:58  17:42:58  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.6  91/01/26  15:13:52  15:13:52  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.5  91/01/25  20:17:33  20:17:33  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.4  91/01/24  17:25:02  17:25:02  db (Dave Berry)
Removed version value.

Revision 1.3  91/01/15  10:59:48  10:59:48  db (Dave Berry)
Renamed "empty" function to "null", added "empty" constant.
Changed type to datatype, so that there is one empty vector for each
type (i.e. each application of the function creates a new empty vector).

Revision 1.2  91/01/10  15:58:25  15:58:25  db (Dave Berry)
List.ge and List.le now take le and ge as arguments instead of
lt and gt.

Revision 1.1  90/12/20  15:01:06  15:01:06  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type Element = Element.T

  datatype MonoArray = Null | Mono of Element Array.Array ref

  type T = MonoArray


(* CONSTANTS *)

  val empty = Null


(* CREATORS *)

  exception Size of string * int

  fun create 0 _ = empty
  |   create size init =
	Mono (ref (Array.create size init))
	handle Array.Size x => raise Size x

  fun tabulate (0, _) = empty
  |   tabulate (size, f) =
	Mono (ref (Array.tabulate (size, f)))
	handle Array.Size x => raise Size x

  fun tabulate' (0, _, _) = empty
  |   tabulate' (size, f, base) =
	Mono (ref (Array.tabulate' (size, f, base)))
	handle Array.Size x => raise Size x


(* ITERATORS *)

  fun map f (Mono (ref a)) = Mono (ref (Array.map f a))
  |   map f Null = Null

  fun apply f (Mono (ref a)) = Array.apply f a
  |   apply f Null = ()

  fun iterate f (Mono (ref a)) = Mono (ref (Array.iterate f a))
  |   iterate f Null = Null

  fun iterateApply f (Mono (ref a)) = Array.iterateApply f a
  |   iterateApply f Null = ()


(* OBSERVERS *)

  fun size (Mono (ref a)) = Array.size a
  |   size Null = 0

  fun isEmpty v = (size v = 0)

  fun eq (Mono r) (Mono r') = (r = r')
  |   eq Null Null = true
  |   eq _ _ = false

  val ne = not oo eq


(* CONVERTORS *)

  fun list (Mono (ref a)) = Array.list a
  |   list Null = []

  fun fromList [] = Null
  |   fromList l = Mono (ref (Array.fromList l))

  fun stringSep start finish sep Null =
	Array.stringSep start finish sep Element.string (Array.fromList [])
  |   stringSep start finish sep (Mono (ref a)) =
	Array.stringSep start finish sep Element.string a

  fun string v =
        if Element.fixedWidth then
          stringSep "" "" "" v
        else
	  stringSep "" "" " " v

  fun printSep os start finish sep Null =
	Array.printSep os start finish sep Element.print (Array.fromList [])
  |   printSep os start finish sep (Mono (ref a)) =
	Array.printSep os start finish sep Element.print a

  fun print os v =
        if Element.fixedWidth then
          printSep os "" "" "" v
        else
	  printSep os "" "" " " v


(* SELECTORS *)

  exception Subscript of string * int
  fun sub (Null, i) = raise Subscript ("Sub", i)
  |   sub (Mono (ref a), i) =
	Array.sub (a, i)
	handle Array.Subscript x => raise Subscript x
  infix 9 sub;

  fun nth n v = v sub n
		handle Subscript _ => raise Subscript ("nth", n)

  exception Extract of int * int
  fun extract start finish Null = raise Extract (start, finish)
  |   extract start finish (Mono (ref a)) =
	Mono (ref (Array.extract start finish a))
	handle Array.Extract x => raise Extract x


(* MANIPULATORS *)

  fun rev (Mono (ref a)) = Mono (ref (Array.rev a))
  |   rev Null = Null

  infix 6 ^
  fun (Mono (ref a)) ^ (Mono (ref a')) = Mono (ref (Array.^ (a, a')))
  |   x ^ Null = x
  |   Null ^ x = x

  exception Update of int
  fun update (Null, i, x) = raise Update i
  |   update (Mono (ref a), i, x)  =
        Array.update (a, i, x)
        handle Array.Update _ => raise Update i

  exception Copy of int * int * int
  fun copy Null start finish _ start' =
	raise Copy (start, finish, start')
  |   copy _ start finish Null start' =
	raise Copy (start, finish, start')
  |   copy (Mono (ref a)) start finish (Mono (ref a')) start' =
  	Array.copy a start finish a' start'
	handle Array.Copy x => raise Copy x

  exception UpdateRange of int * int
  fun updateRange Null start finish i =
	raise UpdateRange (start, finish)
  |   updateRange (Mono (ref a)) start finish i =
  	Array.updateRange a start finish i
        handle Array.UpdateRange x => raise UpdateRange x


(* REDUCERS *)

  exception Empty of string

  fun foldL f b (Mono (ref a)) = Array.foldL f b a
  |   foldL _ b Null = b

  fun foldL' _ Null = raise Empty "foldL'"
  |   foldL' f (Mono (ref a)) = Array.foldL' f a

  fun foldR f b (Mono (ref a)) = Array.foldR f b a
  |   foldR _ b Null = b

  fun foldR' _ Null = raise Empty "foldR'"
  |   foldR' f (Mono (ref a)) = Array.foldR' f a

  fun pairwise f (Mono (ref a)) = Array.pairwise f a
  |   pairwise f Null = true
end
