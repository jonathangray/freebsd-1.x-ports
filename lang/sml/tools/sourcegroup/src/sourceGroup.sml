(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor SourceGroupFun
  (SourceGroupCreate :SOURCE_GROUP_CREATE) :SOURCEGROUP = struct

 val version = 3.0
 open SourceGroupCreate.Data
 structure SourceGroupCore = SourceGroupCreate.SourceGroupCore

 val createInEnv = SourceGroupCreate.createInEnv
 val create = SourceGroupCreate.create
 val make = SourceGroupCore.make
 val makeWhat = SourceGroupCore.makeWhat

 structure Expert :SOURCE_GROUP_EXPERT = SourceGroupCore
end
