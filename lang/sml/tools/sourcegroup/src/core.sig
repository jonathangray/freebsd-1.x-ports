(* Copyright (c) 1992 by Carnegie Mellon University *)

signature SOURCE_GROUP_CORE = sig
 
 include SOURCE_GROUP_EXPERT

 structure Group :GROUP
 structure Data :DATA
 structure IO_Stream :IO_STREAM
 structure Connections :CONNECTIONS
 structure Util :UTIL
 structure Pathname :PATHNAME
 structure Args :ARGS

 sharing Args.Data = Group.Data = Data
 sharing Connections.NameRefTable = Group.NameRefTable
 sharing type group = Data.group
 sharing type sourceInfo = Data.sourceInfo

 val analyzeFiles :string * bool * Data.groupInfo * string list ->
                   Data.fileInfo option list
 val computeDependencies :string -> Data.groupInfo -> unit
 val currencyCheck :Data.groupInfo -> bool * string list
 val make     :Data.group -> string -> unit
 val makeWhat :Data.group -> string -> unit

end
