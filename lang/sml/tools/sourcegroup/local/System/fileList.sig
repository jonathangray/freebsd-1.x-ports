(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature FILELIST = sig
 val inDir :bool * string list -> string list
 val inFile :string list -> string list
 val cshExpansion :string -> string list

 val skipFiles :string list -> string list -> string list
 val extensionsOnly :string list -> string list -> string list
end
