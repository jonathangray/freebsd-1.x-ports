(* Copyright (c) 1992 by Carnegie Mellon University *)

signature SOURCE_GROUP_CREATE = sig
 structure SourceGroupCore :SOURCE_GROUP_CORE
 structure Data :DATA
 sharing SourceGroupCore.Group.Data = Data
 val createInEnv :environment -> Data.groupDescription list -> Data.group
 val create :Data.groupDescription list -> Data.group
end
