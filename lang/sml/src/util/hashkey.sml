signature HASHKEY =
 sig
     type hash
     val new : unit -> hash
     val add : hash * int -> unit
     val get : hash -> string
 end

     
    