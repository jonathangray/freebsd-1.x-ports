(*$Array: ARRAY General List *)

loadSig "ARRAY";

structure Array: ARRAY =

(* ARRAYS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		30 Oct 1989


Maintenance:	Author


SEE ALSO

   ArrayParse, Vector.


RCS LOG

$Log: Array.sml,v $
Revision 1.1  1994/02/08 00:23:20  jkh
Initial revision

Revision 1.15  1991/07/02  15:10:16  db
Bug in copy; missed two arguments when changing order of arguments
for revision 1.12.

Revision 1.14  91/03/12  15:00:35  15:00:35  db (Dave Berry)
Fixed a reference to List.Subscript which should have been
CoreArray.Subscript.

Revision 1.13  91/03/06  16:37:48  16:37:48  db (Dave Berry)
Added print function(s).

Revision 1.12  91/02/22  19:08:34  19:08:34  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.
Uncurried update to match standard agreed for arrays and vectors by the SML
  implementers, and changed the order of arguments of updateRange and copy
  to match (although these functions are still curried).

Revision 1.11  91/02/12  19:38:04  19:38:04  db (Dave Berry)
Removed same and different; made eq and ne test for identity.

Revision 1.10  91/02/11  19:46:13  19:46:13  db (Dave Berry)
Moved read and parse functions to ArrayParse.sml.
Removed comparison and sort functions.
This forms part of the major reorganisation of the library.

Revision 1.9  91/02/04  15:09:46  15:09:46  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.8  91/01/30  18:40:04  18:40:04  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.7  91/01/30  17:42:12  17:42:12  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.6  91/01/28  13:02:09  13:02:09  db (Dave Berry)
Changed implementation to use CoreArray instead of List.
Removed empty constant, since it can't be defined in terms of the
core primitives.

Revision 1.5  91/01/26  15:13:18  15:13:18  db (Dave Berry)
Renamed RefVector and REF_VECTOR to Array and ARRAY, respectively.

Revision 1.4  91/01/25  20:19:36  20:19:36  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/24  17:25:34  17:25:34  db (Dave Berry)
Removed version value.

Revision 1.2  91/01/15  10:59:24  10:59:24  db (Dave Berry)
Renamed "empty" function to "null", added "empty" constant.

Revision 1.1  90/12/20  15:22:00  15:22:00  db (Dave Berry)
Initial revision


*)

struct


(* TYPES *)

  type 'a Array = 'a CoreArray.array

  type 'a T = 'a Array


(* CONVERTERS *)

  fun list a =
	let fun list' i =
	      if i = CoreArray.length a then []
	      else CoreArray.sub (a, i) :: list' (i+1)
	in list' 0
	end

  val fromList = CoreArray.arrayoflist;

  fun stringSep start finish sep p a =
        List.stringSep start finish sep p (list a)
	
  fun string p a =
        List.stringSep "" "" " " p (list a)
	
  fun printSep os start finish sep p a =
        List.printSep os start finish sep p (list a)
	
  fun print os p a =
        List.printSep os "" "" " " p (list a)
	

(* CREATORS *)

  exception Size of string * int

  fun create size init =
        if size < 0 then raise Size ("create", size)
	else fromList (List.create size init)

  fun tabulate (size, f) =
	CoreArray.tabulate (size, f)
	handle CoreArray.Size => raise Size ("tabulate", size)

  fun tabulate' (size, f, base) =
        if size < 0 then raise Size ("tabulate'", size)
	else fromList (List.tabulate' (size, f, base))


(* ITERATORS *)

  fun map f a = fromList (List.map f (list a))

  fun apply f a = List.apply f (list a)

  fun iterate f a = fromList (List.iterate f (list a))

  fun iterateApply f a = List.iterateApply f (list a)


(* OBSERVERS *)

  val size = CoreArray.length

  fun isEmpty v = (size v = 0)

  fun eq a a' = (a = a')

  fun ne a a' = a <> a'


(* SELECTORS *)

  exception Subscript of string * int
  fun sub (a, n) =
        CoreArray.sub (a, n)
        handle CoreArray.Subscript => raise Subscript ("sub", n)
  infix 9 sub;

  fun nth n a = a sub n
		handle Subscript _ => raise Subscript ("nth", n)

  exception Extract of int * int
  fun extract start finish a =
	fromList (List.extract start finish (list a))
        handle List.Extract r => raise Extract r


(* MANIPULATORS *)

  fun rev a = fromList (List.rev (list a))

  infix 6 ^
  fun op ^ (a, a') = fromList (list a @ list a')

  exception Update of int
  fun update (a, i, v) = 
        CoreArray.update (a, i, v)
        handle CoreArray.Subscript => raise Update i

  exception Copy of int * int * int
  local
    fun copy' a start finish a' start' =
	  if start = finish then ()
	  else (update (a', start', (a sub start));
	        copy' a (start + 1) finish a' (start' + 1))
  in
    fun copy a start finish a' start' =
          if finish < start orelse start < 0 orelse finish > size a orelse
	     start' < 0 orelse start' + finish - start > size a'
	  then raise Copy (start, finish, start')
	  else copy' a start finish a' start'
  end
           
  exception UpdateRange of int * int
  local
    fun update' a start finish i =
	  if start = finish then ()
	  else (update (a, start, i);
	        update' a (start + 1) finish i
	       )
  in
    fun updateRange a start finish i =
	  if finish < start orelse start < 0 orelse finish > size a
	  then raise UpdateRange (start, finish)
	  else update' a start finish i
  end


(* REDUCERS *)

  exception Empty of string

  fun foldL f base a =
	List.foldL f base (list a)

  fun foldL' f a =
	if size a = 0 then raise Empty "foldL'"
	else List.foldL' f (list a)

  fun foldR f base a =
	List.foldR f base (list a)

  fun foldR' f a =
	if size a = 0 then raise Empty "foldR'"
	else List.foldR' f (list a)

  fun pairwise f a =
	List.pairwise f (list a)
end
