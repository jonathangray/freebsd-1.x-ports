(* Copyright (c) 1992 by Carnegie Mellon University *)

signature NAMESPACETABLE = sig
 type t
 val create :int -> t
 val enter :t -> (string*int) -> (string*int) -> (string*int) -> unit
 val lookup :t -> (string*int) -> (string*int) -> (string*int) option
 val remove :t -> (string*int) -> (string*int) -> unit
 val eliminate :t->((string*int)->((string*int)->(string*int)->bool))->unit
 val scan :t->((string*int)->unit) -> ((string*int)->(string*int)->unit) ->unit
end
