(*$Hash: HASH Array Int GeneralTypes *)

loadSig "HASH";

structure Hash: HASH = struct

(* HASH TABLE

Created by:  Gene Rollins, School of Computer Science
             Carnegie-Mellon Univ., Pittsburgh, PA 15213
             rollins@cs.cmu.edu
Date:        1 January 1991
Maintenance: None

DESCRIPTION

This hash table module defines operations that expect the caller to supply
both the key and the hash value (the result of applying the hash function to
the key).  One can create a hash table to map any (domain) type to any (range)
type, if at table creation time, one supplies an equality operation on the
domain type.


RCS LOG

$Log: Hash.sml,v $
Revision 1.1  1994/02/08 00:23:17  jkh
Initial revision

Revision 1.3  91/09/13  16:42:12  16:42:12  db (Dave Berry)
Changed arguments of Array.update to match new definition;
Fixed bug in print function.

Revision 1.2  91/03/06  16:38:12  16:38:12  db (Dave Berry)
Added print function(s).

Revision 1.1  91/02/15  17:09:17  17:09:17  db (Dave Berry)
Initial revision


*)



(* LOCAL *)

exception Tl

fun tl (_::t) = t
|   tl _ = raise Tl

infix 9 sub
val op sub = Array.sub

type 'a array = 'a Array.Array


(* IMPLEMENTATION *)

type ('a,'b) table = ('a*'a->bool) * (('a*int*'b) list array) * int

fun create (sample'key :'_a) (equality :'_a * '_a -> bool)
           table'size (sample'value  :'_b ) :('_a,'_b) table =
  let val mt = tl [(sample'key, 0, sample'value)]
  in (equality, Array.create table'size mt, table'size)
  end

val defaultSize = 97 (* a prime; or try primes 37, 997 *)

fun defaultEqual ((x :string), (y :string)) :bool = (x = y)

(* The use of CoreArray.array instead of Array.create in the following
   function gets round a bug in SML/NJ. *)
fun createDefault (sample'value :'_b) :(string,'_b) table =
  let val mt = tl [("", 0, sample'value)]
  in (defaultEqual, CoreArray.array (defaultSize, mt), defaultSize)
  end

fun enter ((equal, table, table'size) :('a,'b) table) key hash value = 
  let val place = hash mod table'size
      val bucket = table sub place
      fun put'in [] = [(key,hash,value)]
        | put'in ((k,h,v)::tail) =
	    if (h = hash) andalso equal (k, key)
	      then (key,hash,value)::tail
	      else (k,h,v)::(put'in tail)
  in
    Array.update (table, place, (put'in bucket))
  end

fun remove ((equal, table, table'size) :('a,'b) table) key hash =
  let val place = hash mod table'size
      val bucket = table sub place
      fun take'out [] = []
        | take'out ((k,h,v)::tail) =
	    if (h = hash) andalso equal (k, key)
	      then tail
	      else (k,h,v)::(take'out tail)
  in
    Array.update (table, place, (take'out bucket))
  end

fun lookup ((equal, table, table'size) :('a,'b) table) key hash =
  let val place = hash mod table'size
      val bucket = table sub place
      fun get'out [] = None
        | get'out ((k,h,v)::tail) =
	    if (h = hash) andalso equal (k, key)
	      then Some v
	      else get'out tail
  in
    get'out bucket
  end


fun print (os :outstream) ((_, table, table'size) :('a,'b) table)
           (print'key :outstream -> 'a -> unit)
	   (print'value :outstream -> 'b -> unit) =
  let fun pr'bucket [] = ()
        | pr'bucket ((key,hash,value)::rest) =
            (print'key os key; output (os, ": ");
             Int.print os hash; output (os, ": ");
	     print'value os value; output (os, "\n"); pr'bucket rest)
      fun pr i =
        if i >= table'size then ()
	  else
	    case (table sub i) of
	       [] => (pr (i+1))
             | (b as (h::t)) =>
	         (output (os, "["); Int.print os i; output (os, "]\n");
	          pr'bucket b; pr (i+1))
  in pr 0 end


fun string ((_, table, table'size) :('a,'b) table)
           (string'key :'a -> string) (string'value :'b -> string) =
  let fun pr'bucket [] = ""
        | pr'bucket ((key,hash,value)::rest) =
            (string'key key ^ ": " ^
             Int.string hash ^ ": " ^
	     string'value value ^ "\n" ^ pr'bucket rest)
      fun pr i =
        if i >= table'size then ""
	  else
	    case (table sub i) of
	       [] => (pr (i+1))
             | (b as (h::t)) =>
	         ("[" ^ Int.string i ^ "]\n" ^
	          pr'bucket b ^ pr (i+1))
  in pr 0 end


fun scan ((_, table, table'size) :('a,'b) table) operation =
  let fun map'bucket [] = ()
        | map'bucket ((key,hash,value)::rest) =
            (operation key hash value; map'bucket rest)
      fun iter i =
        if i >= table'size then ()
	  else (map'bucket (table sub i); iter (i+1))
  in iter 0 end

fun fold ((_, table, table'size) :('a, 'b) table)
         (operation :'a -> int -> 'b -> 'g -> 'g) (init :'g) :'g =
  let fun fold'bucket [] acc = acc
        | fold'bucket ((key,hash,value)::rest) acc =
             fold'bucket rest (operation key hash value acc)
      fun iter i acc =
        if i >= table'size then acc
	  else iter (i+1) (fold'bucket (table sub i) acc)
  in iter 0 init end

fun scanUpdate ((_, table, table'size) :('a,'b) table) operation =
  let fun map'bucket [] = []
        | map'bucket ((key,hash,value)::rest) =
            ((key,hash,operation key hash value)::(map'bucket rest))
      fun iter i =
        if i >= table'size then ()
	  else (Array.update (table, i, (map'bucket (table sub i))); iter (i+1))
  in iter 0 end

fun eliminate ((_, table, table'size) :('a,'b) table) predicate =
  let fun map'bucket [] = []
        | map'bucket ((key,hash,value)::rest) =
            if predicate key hash value then map'bucket rest
              else (key,hash,value)::(map'bucket rest)
      fun iter i =
        if i >= table'size then ()
	  else (Array.update (table, i, (map'bucket (table sub i))); iter (i+1))
  in iter 0 end

fun bucketLengths ((_, table, table'size) :('a,'b) table) (maxlen :int)
    :int array =
  let val count :int array = Array.create (maxlen+1) 0
      fun inc'sub x = 
        let val y = Int.min x maxlen in
          Array.update (count, y, (count sub y + 1))
        end
      fun iter i =
        if i >= table'size then ()
	  else (inc'sub (CoreUtils.length (table sub i)); iter (i+1))
  in
    iter 0;
    count
  end

end

