(* Copyright (c) 1992 by Carnegie Mellon University *)

signature MODTABLE = sig

type t
val create     :environment -> t
val lookup     :t -> (string * (string * int)) -> bool
val pervasives :t

end
