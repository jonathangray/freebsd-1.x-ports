(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature HASH =
  sig
    type ('a,'b) table
    val create : ('1a * '1a -> bool) -> int -> '1b list -> ('1a,'1b) table
    val createDefault : '1b list -> (string,'1b) table
    val defaultEqual : string * string -> bool
    val defaultSize : int
    val enter : ('a,'b) table -> ('a * int) -> 'b -> unit
    val lookup : ('a,'b) table -> ('a * int) -> 'b option
    exception NotFound
    val lookup' : ('a,'b) table -> ('a * int) -> 'b
    val eliminate : ('a,'b) table -> (('a * int) -> 'b -> bool) -> unit
    val print : ('a,'b) table -> ('a -> unit) -> ('b -> unit) -> unit
    val remove : ('a,'b) table -> ('a * int) -> unit
    val scan : ('a,'b) table -> (('a * int) -> 'b -> unit) -> unit
    val scanUpdate : ('a,'b) table -> (('a * int) -> 'b -> 'b) -> unit
    val fold : ('a,'b) table -> (('a * int) -> 'b -> 'g -> 'g) -> 'g -> 'g
    val bucketLengths : ('a,'b) table -> int -> int array
  end
