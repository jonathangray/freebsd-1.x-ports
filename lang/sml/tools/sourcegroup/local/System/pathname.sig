(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

signature PATHNAME = sig
 val getwd :unit -> string

 val clearPath   :string -> string
 val clearPath'  :string list -> string list
 val explodePath :string -> string list
 val implodePath :string list -> string
 
 val extension          :string -> string
 val stripExtension     :string -> string
 val splitFileExtension :string -> string * string
 
 val directoryPart  :string -> string
 val stripDirectory :string -> string
 val mergeDirFile   :string -> string -> string
 val mergePathnames :string list -> string
 val splitDirFile   :string -> string * string
 val splitDirFile'  :string -> string * string
 
 val resolveSymLinks     :string -> string
 val resolveSymLinks'    :string * string -> string
 val resolveAllSymLinks  :string -> string
 val resolveAllSymLinks' :string list -> string

 val relativeName  :string -> string -> string
 val absoluteName  :string -> string -> string
 val absoluteName' :string list -> string list -> string list
 val absoluteRealPathname  :string -> string
 val absoluteRealPathname' :string list -> string
end
