(* Copyright (c) 1992 by Carnegie Mellon University *)

functor ArgsFun (Group :GROUP) :ARGS = struct

structure Data = Group.Data
open Data

fun getSourceFiles (d:groupDescription, acc:string list) :string list =
  (case d of
      (SubGroups _) => acc
    | (Connections _) => acc
    | (Sources namelist) => acc @ namelist)

fun getSubGroups (d:groupDescription, acc:groupInfo list) :groupInfo list =
  (case d of
      (Sources _) => acc
    | (Connections _) => acc
    | (SubGroups groups) => acc @ (map Group.findGroup groups))

fun getConnFileList (d:groupDescription, acc:string list) :string list =
  (case d of
      (SubGroups _) => acc
    | (Sources _) => acc
    | (Connections namelist) => acc @ namelist)

fun rearrange (description :groupDescription list)
   :string list * groupInfo list * string list =
  let val revDescription = List.rev description
      val sourceFiles = fold getSourceFiles revDescription []
      val subGroups = fold getSubGroups revDescription []
      val connFiles = List.rev (fold getConnFileList revDescription [])
  in
    (sourceFiles, subGroups, connFiles)
  end

end
