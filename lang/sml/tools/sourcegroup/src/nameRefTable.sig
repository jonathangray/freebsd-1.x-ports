(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins
   School of Computer Science
   Carnegie-Mellon University
   Pittsburgh, PA 15213
   rollins@cs.cmu.edu *)

signature NAMEREFTABLE =
  sig
    type t
    val create : int -> t
    val enter : t -> (string*int) -> string list -> unit
    val lookup : t -> (string*int) -> string list option
    val nestedFold : t -> ((string*int) -> (string*'g -> 'g)) -> 'g -> 'g
    val nestedScan : t -> ((string*int) -> (string -> unit)) -> unit
    val append : t -> (string*int) -> string -> unit
    val getNameList : t -> (string*int) -> (string list)
    val scan : t -> ((string*int) -> string list -> unit) -> unit
    val fold : t -> ((string*int) -> string list -> 'g -> 'g) -> 'g -> 'g
  end
