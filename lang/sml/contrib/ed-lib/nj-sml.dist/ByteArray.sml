(*$ByteArray : Byte List GeneralTypes InStreamType OutStreamType *)

structure ByteArray: MONO_ARRAY =

(* ARRAYS OF BYTES

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		12 Feb 1990

Maintenance:	Author

DESCRIPTION

   A ByteArray is a unique sequence of bytes that can be updated in
   place.  There is only one empty ByteArray.


RCS LOG

$Log: ByteArray.sml,v $
Revision 1.1  1994/02/08 00:23:13  jkh
Initial revision

Revision 1.1  91/09/13  17:16:01  17:16:01  db (Dave Berry)
Initial revision



*)

struct


(* TYPES *)

  type Element = Byte.Byte

  type MonoArray = ByteArray.bytearray

  type T = MonoArray


(* CONVERTORS *)

  fun list v =
    let val n = ByteArray.length v
	fun list' i =
    	  if i = n then []
    	  else ByteArray.sub (v, i) :: list' (i+1)
    in list' 0
    end

  fun fromList l =
	let val v = ByteArray.array (List.size l, 0)
	    fun fromList' [] _ = ()
	    |   fromList' (h::t) i =
		  ( ByteArray.update (v, i, h);
		    fromList' t (i+1)
		  )
	    val _ = fromList' l 0
	in v
	end


(* CONSTANTS *)

  val empty = fromList []


(* CREATORS *)

  exception Size of string * int

  fun create size init =
	if size < 0 then raise Size ("create", size)
	else ByteArray.array (size, init)

  fun tabulate (size, f) =
	if size < 0 then raise Size ("tabulate", size)
	else fromList (List.tabulate (size, f))

  fun tabulate' (size, f, base) =
	if size < 0 then raise Size ("tabulate'", size)
        else fromList (List.tabulate' (size, f, base))


(* ITERATORS *)

  fun map f v = fromList (List.map f (list v))

  fun apply f v = ByteArray.app f v

  fun iterate f v = fromList (List.iterate f (list v))

  fun iterateApply f v = List.iterateApply f (list v)


(* OBSERVERS *)

  val size = ByteArray.length

  fun isEmpty v = (size v = 0)

  fun eq v v' = (v = v')

  fun ne v v' = (v <> v')


(* CONVERTORS *)

  fun stringSep start finish sep v =
        List.stringSep start finish sep Byte.string (list v)

  fun string v = stringSep "" "" "" v

  fun printSep os start finish sep v =
	List.printSep os start finish sep Byte.print (list v)

  fun print os v = printSep os "" "" "" v


(* SELECTORS *)

  exception Subscript of string * int

  fun sub (v, n) =
	ByteArray.sub (v, n)
	handle ByteArray.Subscript => raise Subscript ("sub", n)
  infix 9 sub;

  fun nth n v = v sub n
		handle Subscript _ => raise Subscript ("nth", n)

  exception Extract of int * int
  fun extract start finish v =
	fromList (List.extract start finish (list v))
	handle List.Extract x => raise Extract x


(* MANIPULATORS *)

  fun rev v = fromList (List.rev (list v))

  infix 6 ^
  fun op ^ (v, v') = fromList (list v @ list v')

  exception Update of int
  fun update (v, i, x) =
	ByteArray.update (v, i, x)
	handle ByteArray.Subscript => raise Update i

  exception Copy of int * int * int
  local
    fun copy' start finish v start' v' =
          if start = finish then ()
          else (update (v', start', (v sub start));
                copy' (start + 1) finish v (start' + 1) v')
  in
    fun copy v start finish v' start' =
          if finish < start orelse start < 0 orelse finish > size v orelse
             start' < 0 orelse start' + finish - start > size v'
          then raise Copy (start, finish, start')
          else copy' start finish v start' v'
  end

  exception UpdateRange of int * int
  local
    fun update' start finish i v =
          if start = finish then ()
          else (update (v, start, i);
                update' (start + 1) finish i v)
  in
    fun updateRange v start finish i =
          if finish < start orelse start < 0 orelse finish > size v
          then raise UpdateRange (start, finish)
          else update' start finish i v
  end


(* REDUCERS *)

  exception Empty of string

  fun foldL f base v =
	ByteArray.fold (General.uncurry f) v base

  fun foldL' f v =
	if size v = 0 then raise Empty "foldL'"
	else List.foldL' f (list v)

  fun foldR f base v =
	ByteArray.revfold (General.uncurry f) v base

  fun foldR' f v =
	if size v = 0 then raise Empty "foldR'"
	else List.foldR' f (list v)

  fun pairwise f v =
	List.pairwise f (list v)
end
