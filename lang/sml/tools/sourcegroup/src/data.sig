(* Copyright (c) 1992 by Carnegie Mellon University *)

signature DATA = sig

structure Hash :HASH
structure Hasher :HASHER
structure NameRefTable :NAMEREFTABLE
structure NamespaceTable :NAMESPACETABLE
structure ModTable :MODTABLE

type group
exception CompilingError
val namespaceSize :int

datatype sourceInfo = Source of
  {sourceName     :string,
   targetName     :string,
   group          :group,
   toolName       :string,
   envCurrent     :bool,
   targetCurrent  :bool,
   dependsOn      :string list,
   loadSource     :unit -> unit,
   genTarget      :unit -> unit,
   loadTarget     :unit -> unit,
   checkLoad      :unit -> unit,
   compileSource  :unit -> unit}

datatype groupDescription =
   Sources of string list
 | SubGroups of group list
 | Connections of string list

datatype fileInfo = F of
  {nameH          :string*int,
   toolH          :string*int,
   infoTime       :System.Timer.time ref,
   envObjectTime  :System.Timer.time ref,
   envUpdateTime  :System.Timer.time ref,
   imports        :NameRefTable.t ref,
   exports        :NameRefTable.t ref,
   env'current    :bool ref,
   target'current :bool ref, 
   target'name    :string ref,
   index          :bool ref,
   depends        :(bool*(string*int)) list ref}

datatype groupInfo = G of
  {filetable      :(string, fileInfo) Hash.table ref,
   groupId        :group ref,
   initialized    :int ref,
   namespaces     :NamespaceTable.t ref,
   main'groups    :groupInfo list ref,
   lib'groups     :groupInfo list ref,
   target'name'of :string -> string,
   indexing       :bool,
   connFile       :string option ref,
   modtable       :ModTable.t,
   infoTime       :System.Timer.time ref}
end
