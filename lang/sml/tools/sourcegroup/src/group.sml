(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor GroupFun
  (structure Util :UTIL sharing type Util.Data.group=int
   structure ToolInternals :TOOL_INTERNALS
   structure DirFile :DIRFILE
  ) :GROUP = struct

 structure Util = Util
 structure Data = Util.Data
 structure ModTable = Data.ModTable
 structure Hash = Data.Hash
 structure Hasher = Data.Hasher
 structure NameRefTable = Data.NameRefTable
 structure NamespaceTable = Data.NamespaceTable

open Data
val say = System.Print.say
val hasher = Hasher.hasher
val warn = Util.warn
val modtime = DirFile.timeModified

exception SourceGroupInternalError

val smlH = hasher "sml"

val nextGroupId = ref 0
fun groupEqual (x:group, y:group) = (x = y)
val groupTable = Hash.create groupEqual 37 ([]:groupInfo list)
val allFiles = Hash.createDefault ([]:fileInfo list)

fun groupId (group as G {groupId=group_id,...} :groupInfo) :group = (!group_id)

fun findGroup (g:group) =
  case Hash.lookup groupTable (g,g) of
     NONE => (say "? SourceGroup: group not found\n";
              raise CompilingError)
   | (SOME group'info) => group'info

fun enterGroup (group'info as G {groupId,...} :groupInfo) :group =
  let val g = !nextGroupId in
    Hash.enter groupTable (g,g) group'info;
    groupId := g;
    nextGroupId := g + 1;
    (g :group)
  end

fun doLib
      (Source{envCurrent,targetCurrent,loadSource,loadTarget,...}:sourceInfo)=
  if envCurrent then ()
    else if targetCurrent then loadTarget() else loadSource()

fun newGroup (isMain:bool)(modtable:ModTable.t)(connFileOption :string option)
        (mainGroups :groupInfo list) (libGroups :groupInfo list) :groupInfo =
  let val defTime =
            case connFileOption of NONE => Util.currentTime()
             | (SOME connFile) => modtime connFile
      val spaceSize = if isMain then 1 else Data.namespaceSize
      val ToolInternals.Tool {targetNamer,...} = ToolInternals.getToolInfo smlH
      val group :groupInfo =
        G {filetable =
            if isMain
              then ref (Hash.create Util.stringEqual 1 ([]:fileInfo list))
              else ref (Hash.createDefault ([]:fileInfo list)),
           groupId=ref (0:group), initialized=ref 0, modtable=modtable,
           namespaces=ref(NamespaceTable.create spaceSize),
           main'groups=ref mainGroups, lib'groups=ref libGroups,
           target'name'of=(!targetNamer), indexing=(!System.Control.indexing),
           connFile=ref connFileOption, infoTime=ref defTime} :groupInfo
  in
    enterGroup group;
    group
  end

fun newMainGroup (modtable:ModTable.t)
        (mainGroups :groupInfo list) (libGroups :groupInfo list) :groupInfo =
  newGroup true modtable NONE mainGroups libGroups

fun newSubGroup (connFileOption :string option) :groupInfo =
  newGroup false (ModTable.pervasives) connFileOption [] []

fun addFile (G{filetable,...}:groupInfo) filenameH = 
  Hash.enter (!filetable) filenameH
fun foldFiles (G {filetable,...}:groupInfo) operation accum =
  Hash.fold (!filetable) operation accum
fun scanFiles (G {filetable,...}:groupInfo) operation =
  Hash.scan (!filetable) operation
fun clearFiles (G {filetable,...}:groupInfo) = 
  (filetable := Hash.createDefault ([]:fileInfo list))

fun lookupFile filenameH = Hash.lookup allFiles filenameH

fun lookupFile' (filenameH as (filename,_)) =
  case Hash.lookup allFiles filenameH of
     NONE =>
       (say ("? file \""^filename^"\" not found in groups.");
        raise SourceGroupInternalError)
   | (SOME info) => info

fun libraryGroups (G {lib'groups,...}:groupInfo) = !lib'groups

fun connFile (G {connFile,...}:groupInfo) = !connFile
fun setConnFile (G {connFile,...}:groupInfo) name = (connFile := name)

fun infoTime (G {infoTime,...}:groupInfo) = !infoTime
fun setInfoTime (G {infoTime,...}:groupInfo) newTime = (infoTime := newTime)

fun groupsFold (group:groupInfo) (isLibraryGroup:bool)
               (opr :groupInfo->bool->'a->'a) (acc:'a) =
  let fun do'group (g as G{main'groups,lib'groups,...}:groupInfo)
                   (isLibrary:bool) (ac:'a) =
        let val result0 = opr g isLibrary ac
            val result1 = iter (!main'groups) isLibrary result0
        in
          iter (!lib'groups) true result1
        end
      and iter [] (isLibrary:bool) (accum:'a) = accum
        | iter (head::tail) (isLibrary:bool) (accum:'a) =
            let val result0 = do'group head isLibrary accum in
              iter tail isLibrary result0
            end
  in
    do'group group isLibraryGroup acc
  end

fun lookupThruGroups
      (group:groupInfo) spaceH nameH :(bool*(string*int)) option =
  let exception Done of (bool*(string*int)) option
      fun lookup (g as G {namespaces,...}:groupInfo)
                 (isLibrary:bool) (acc:(bool*(string*int)) option) =
        (case NamespaceTable.lookup (!namespaces) spaceH nameH of
            (SOME (filenameH as (filename,_))) =>
              (case lookupFile filenameH of
                  NONE => 
                    (warn ["file ", filename, " not found in group tables"];
                     raise (Done NONE))
                | (SOME info) => 
                    raise (Done (SOME (isLibrary, filenameH))))
          | NONE => NONE)
  in
    (groupsFold group false lookup NONE)
      handle (Done r) => r
  end

fun checkDefineName (group as G {namespaces,...}:groupInfo)
                    update (filenameH as (filename,_))
                    (spaceH as (space,_)) name =
  let val nameH = hasher name in
    case lookupThruGroups group spaceH nameH of
       NONE => NamespaceTable.enter (!namespaces) spaceH nameH filenameH
     | (SOME ((_,(fname,_)):bool*(string*int))) =>
         if update
           then NamespaceTable.enter (!namespaces) spaceH nameH filenameH
         else 
            (warn [space," ",name," redefined;",
                 "\n  Originally defined in file ", fname,
                 "\n  Using definition from ", filename];
             NamespaceTable.enter (!namespaces) spaceH nameH filenameH)
  end

exception SourceGroup_InternalError
exception Skip
val smlH = hasher "sml"

fun checkTool toolName filename =
  if toolName="" then smlH 
    else let val toolH = hasher toolName in
           if !Util.toolIsDefined toolH
             then toolH
             else (warn ["Tool ",toolName,
                            " undefined; ignoring source file ",filename];
                   raise Skip)
         end
                                       
fun defineSource
      (group as G{filetable,target'name'of,indexing,...}:groupInfo,
       definition'time:System.Timer.time, redefine:bool)
      (toolName:string, filename:string,
       new'imports:NameRefTable.t, new'exports:NameRefTable.t) =
  let val toolH = checkTool toolName filename
      val filenameH = hasher filename
  in
    case Hash.lookup allFiles filenameH of
       NONE =>
         let val info =
            (F {imports=ref new'imports, exports=ref new'exports,
                nameH=filenameH, depends=ref[], infoTime=ref definition'time, 
                envObjectTime=ref Util.zeroTime,
                envUpdateTime=ref Util.zeroTime, 
                target'name=ref(target'name'of filename), index=ref indexing,
                toolH=toolH, target'current=ref false, env'current=ref false})
         in
           Hash.enter allFiles filenameH info;
           Hash.enter (!filetable) filenameH info;
           (SOME info)
         end
     | (SOME(info as F {imports,exports,infoTime,target'name,index,...})) =>
         (imports := new'imports;
          exports := new'exports;
          infoTime := definition'time;
          if redefine then () else
            (target'name := target'name'of filename;
             index := indexing);
          case Hash.lookup (!filetable) filenameH of
             NONE => Hash.enter (!filetable) filenameH info
           | (SOME _) => ();
          (SOME info))
  end handle Skip => NONE

fun enterExports group infoOption =
  case infoOption of
     NONE => ()
  | (SOME (F {nameH,exports,...})) =>
      NameRefTable.nestedScan (!exports) (checkDefineName group true nameH)

fun updateNamespaces
      (group as G {namespaces,infoTime,...}:groupInfo) changedFiles =
  let val changedFileSet =
            let val s = Hash.createDefault ([]:unit list)
                fun enterFile infoOption =
                      case infoOption of
                         NONE => ()
                       | (SOME (F {nameH,...})) => Hash.enter s nameH ()
            in map enterFile changedFiles; s end
      fun isChangedFile spaceH nameH filenameH =
        case Hash.lookup changedFileSet filenameH of
           NONE => false | (SOME _) => true
  in
    NamespaceTable.eliminate (!namespaces) isChangedFile;
    map (enterExports group) changedFiles;
    infoTime := Util.currentTime ()
  end

fun createNamespaces (firstTime:bool)
      (group as G {namespaces,infoTime,...}:groupInfo) infoList =
  (if firstTime then () else namespaces := NamespaceTable.create namespaceSize;
   map (enterExports group) infoList;
   infoTime := Util.currentTime())
    
fun updateEnv
      (group:group, sourceName:string, targetName:string, updateSource:bool) =
  case lookupFile (hasher sourceName) of
     NONE => ()
   | (SOME (F {envUpdateTime,envObjectTime,...})) =>
       (envUpdateTime := Util.currentTime ();
        envObjectTime :=
          modtime (if updateSource then sourceName else targetName))

end
