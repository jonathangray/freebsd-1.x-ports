(* Copyright (c) 1992 by Carnegie Mellon University *)

signature GROUP = sig
  structure Data :DATA
  structure NameRefTable :NAMEREFTABLE
  structure Util :UTIL
  structure ModTable :MODTABLE

  sharing Data = Util.Data
  sharing ModTable = Data.ModTable
  sharing NameRefTable = Data.NameRefTable

  val groupId :Data.groupInfo -> Data.group
  val groupEqual :Data.group * Data.group ->bool
  val findGroup :Data.group ->Data.groupInfo
  val newMainGroup :ModTable.t -> Data.groupInfo list -> Data.groupInfo list
                     -> Data.groupInfo
  val newSubGroup :string option -> Data.groupInfo

  val addFile :Data.groupInfo ->string*int ->Data.fileInfo ->unit
  val foldFiles
        :Data.groupInfo ->(string*int ->Data.fileInfo ->'a ->'a) ->'a ->'a
  val scanFiles :Data.groupInfo ->(string*int ->Data.fileInfo ->unit) ->unit
  val clearFiles :Data.groupInfo ->unit

  val lookupFile  :string * int -> Data.fileInfo option
  val lookupFile' :string * int -> Data.fileInfo

  val libraryGroups :Data.groupInfo ->Data.groupInfo list

  val connFile :Data.groupInfo ->string option
  val setConnFile :Data.groupInfo ->string option ->unit
  val infoTime :Data.groupInfo ->System.Timer.time
  val setInfoTime :Data.groupInfo ->System.Timer.time ->unit

  val groupsFold
        :Data.groupInfo ->bool ->(Data.groupInfo ->bool ->'a ->'a) ->'a ->'a
  val lookupThruGroups 
        :Data.groupInfo->string*int->string*int -> ((bool*(string*int)) option)
  val checkDefineName
        :Data.groupInfo ->bool ->string*int ->string*int ->string ->unit
  val defineSource
        :(Data.groupInfo * System.Timer.time * bool) ->
         (string*string*NameRefTable.t*NameRefTable.t) -> Data.fileInfo option

  val createNamespaces :bool->Data.groupInfo->(Data.fileInfo option list)->unit
  val updateNamespaces :Data.groupInfo ->(Data.fileInfo option list) ->unit

  val updateEnv     :Data.group * string * string * bool -> unit
end
