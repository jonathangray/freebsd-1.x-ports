(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature STRINGXTRA = sig
 exception getIntError
 val findChr : string -> int * string -> int
 val findChrFromRight : string -> int * string -> int
 val getInt : int * string -> int * int
 val getWord : int * string -> int * string
 val isBlankChr : string -> bool
 val isIdChr : string -> bool
 val skipBlanks : int * string -> int
 val stringEqual : string * string -> bool
 val stringListEqual : string list -> string list -> bool
 val printl :string list -> unit
 val printSep :string list -> string -> unit
 val stringListPrint :string list -> string -> unit
 val breakAtBlanks :string -> string list
end
