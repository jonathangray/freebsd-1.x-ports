(*$HASH *)

signature HASH =
sig

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

$Log: HASH.sml,v $
Revision 1.1  1994/02/08 00:23:26  jkh
Initial revision

Revision 1.2  91/03/06  16:28:54  16:28:54  db (Dave Berry)
Added print function(s).

Revision 1.1  91/02/15  17:10:11  17:10:11  db (Dave Berry)
Initial revision


*)

   type ('a,'b) table

   val create : '_a -> ('_a * '_a -> bool) -> int -> '_b -> ('_a, '_b) table
(* fun create (sample'key :'_a) (equality :'_a * '_a -> bool)
              (table'size :int) (sample'value :'_b) :('_a,'_b) table
Create a hash table mapping 'a to 'b. *)

   val defaultSize : int
(* val defaultSize = 97 *)

   val defaultEqual : string * string -> bool
(* fun defaultEqual ((x :string), (y :string)) :bool
Regular string equality (x=y). *)

   val createDefault : '_a -> (string, '_a) table
(* fun createDefault (sample'value :'_b) :(string,'_b) table
Create a hash table mapping string to 'b; use defaultSize and defaultEqual. *)

   val enter : ('a,'b) table -> 'a -> int -> 'b -> unit
(* fun enter (hash'table :('a,'b) table) (key :'a) (hash :int) (value :'b)
Alter hash'table to map key to value. *)

   val remove : ('a,'b) table -> 'a -> int -> unit
(* fun remove (hash'table :('a,'b) table) (key :'a) (hash :int)
Remove key from the mapping. *)

   val lookup : ('a,'b) table -> 'a -> int -> 'b Option
(* fun lookup (hash'table :('a,'b) table) key hash :'b Option
Return the value that hash'table maps key into. *)

   val string : ('a,'b) table -> ('a -> string) -> ('b -> string) -> string
(* fun string (hash'table :('a,'b) table)
           (string'key :'a -> unit) (string'value :'b -> unit)
Return a string representation of the hash'table mapping, including bucket
indexes, keys, hash values, and range values. *)

   val print : outstream -> ('a,'b) table -> (outstream -> 'a -> unit) ->
	       (outstream -> 'b -> unit) -> unit
(* fun print (os: outstream) -> (hash'table :('a,'b) table)
           (print'key :outstream -> 'a -> unit)
	   (print'value :outstream -> 'b -> unit)
Print the hash'table mapping, including bucket indexes, keys, hash values,
and range values, on the stream os. *)

   val scan : ('a,'b) table -> ('a -> int -> 'b -> unit) -> unit
(* fun scan (hash'table :('a,'b) table) (operation :'a -> int -> 'b -> unit)
Apply operation to every entry in the table.  Arguments to operation are
key, hash value, domain value. *)

   val fold : ('a,'b) table -> ('a -> int -> 'b -> 'g -> 'g) -> 'g -> 'g
(* fun fold (hash'table :('a, 'b) table)
         (operation :'a -> int -> 'b -> 'g -> 'g) (init :'g) :'g
Apply operation to every entry in the table and an accumulated value.  The
accumulated value is initially init.  The result of applying operation
to one table entry is passed as the accumulated value for the next
application of operation.  The result of the last application of
operation is returned as the result of fold. *)

   val scanUpdate : ('a,'b) table -> ('a -> int -> 'b -> 'b) -> unit
(* fun scanUpdate (hash'table:('a,'b) table) (operation:'a -> int -> 'b -> 'b)
Apply operation to every entry in hash'table, and replace the domain value
for the entry with the result of operation. *)

   val eliminate : ('a,'b) table -> ('a -> int -> 'b -> bool) -> unit
(* fun eliminate (hash'table:('a,'b) table) (predicate:'a->int->'b->bool)
Apply predicate to every entry in hash'table.  If the predicate is true,
then remove the entry from the table. *)

   val bucketLengths : ('a,'b) table -> int -> int Array.Array
(* fun bucketLengths (hash'table :('a,'b) table) (maxlen :int) :int Array.Array
Creates an array with domain 0 through maxlen.  Counts the number of buckets in
hash'table of every length 0 through maxlen-1, putting those counts into
appropriate slots in the result array.  Also counts the number of buckets whose
length is maxlen or greater and puts that count into the maxlen slot of the
result array. *)

end

