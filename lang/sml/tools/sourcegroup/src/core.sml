(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor SourceGroupCoreFun
  (structure DirFile :DIRFILE
   structure ListSort :LISTSORT
   structure Connections :CONNECTIONS
   structure Group :GROUP
   structure Args :ARGS
   structure ToolInternals :TOOL_INTERNALS
   structure Analyze :ANALYZE
   structure StringXtra :STRINGXTRA
   structure IO_Stream :IO_STREAM
   structure Auxiliary :AUXILIARY 
   structure Pathname :PATHNAME
   structure Interrupt :INTERRUPT

   sharing Group.Data = Args.Data = ToolInternals.Data = Auxiliary.Data
   sharing Connections.NameRefTable = Group.NameRefTable
   sharing type Group.Data.group = int
  ) = struct

 structure Group = Group
 structure Util = Group.Util
 structure Data = Group.Data
 structure ModTable = Data.ModTable
 structure NameRefTable = Group.NameRefTable
 structure ToolInternals = ToolInternals
 structure Hash = Data.Hash
 structure Hasher = Data.Hasher
 structure IO_Stream = IO_Stream
 structure Connections = Connections
 structure Pathname = Pathname
 structure Args = Args

open Data
type time = System.Timer.time

exception SourceGroupInternalError
exception Skip
exception Quit

val say = System.Print.say

fun doCompile (Source {envCurrent, targetCurrent,
                       loadTarget, compileSource,...} :sourceInfo) =
 if targetCurrent
   then if envCurrent then () else loadTarget ()
   else compileSource ()

fun doPrint (Source {sourceName, targetName, toolName,
                     envCurrent, targetCurrent,...} :sourceInfo) =
  if targetCurrent
    then if envCurrent then () else
      (case toolName of
          "yacc" => () | "lex" => ()
        | _ => (print "loadTarget \""; print targetName; print "\";\n"))
    else (print "compile \""; print sourceName; print "\";\n")

val libraryAction = ref doCompile
val makeLibraries = ref true
val issueWarnings = Util.issueWarnings
val quietCreation = ref false
val continueAfterError = ref true

val hasher = Hasher.hasher
val smlH = hasher "sml"
val signatureH =  hasher "signature"
val functorH = hasher "functor"
val structureH = hasher "structure"
val funsigH = hasher "funsig"
val fileH = hasher "file"

val absoluteName = Pathname.absoluteName
val relativeName = Pathname.relativeName
val getwd = Pathname.getwd
val newAs = Util.newAs
val modtime = DirFile.timeModified
val warn = Util.warn

fun string'equal ((x, y):string*string) :bool = (x = y)
val normalize = ListSort.unique'sort string'equal String.<
val lookupFile' = Group.lookupFile'

type fileref = bool * (string * int)
fun filerefName ((_,(name,_)):fileref) :string = name
fun filerefNameEqual (((_,(a,_)):fileref),((_,(b,_)):fileref)) = (a=b)
fun filerefNameLess (((_,(a,_)):fileref),((_,(b,_)):fileref)) = (a<b)
val filerefNormalize = ListSort.unique'sort filerefNameEqual filerefNameLess

fun echoFilename (cwd:string) (filename:string) =
  if !quietCreation then () else say ((relativeName cwd filename)^"\n")

fun analysisError (cwd:string, redefine:bool, filename:string) =
 (if redefine
    then warn ["Parsing error in file ", relativeName cwd filename,
                  "; assuming no interface changes"]
    else warn ["Parsing error in file ", relativeName cwd filename,
                  "; not included in group"];
  raise Skip)

fun analyzeConnections (cwd:string, redefine:bool, filename :string) =
  let val ((strIm, sigIm, funIm, fsigIm), (strEx, sigEx, funEx, fsigEx)) =
        (Analyze.connections (relativeName cwd filename))
          handle _ => analysisError (cwd, redefine, filename)
      val importTable = NameRefTable.create namespaceSize
      val exportTable = NameRefTable.create namespaceSize
  in
    NameRefTable.enter importTable signatureH sigIm;
    NameRefTable.enter importTable structureH strIm;
    NameRefTable.enter importTable functorH   funIm;
    NameRefTable.enter importTable funsigH    fsigIm;
    NameRefTable.enter exportTable signatureH sigEx;
    NameRefTable.enter exportTable structureH strEx;
    NameRefTable.enter exportTable functorH   funEx;
    NameRefTable.enter exportTable funsigH    fsigEx;
    (importTable, exportTable)
  end

fun getConnections (cwd:string, redefine:bool, filename :string) =
  case Group.lookupFile (hasher filename) of
     NONE => analyzeConnections (cwd, redefine, filename)
   | (SOME (info as F {imports,exports,infoTime,...})) =>
       if Util.newer (modtime filename, !infoTime)
         then analyzeConnections (cwd, redefine, filename)
         else (!imports, !exports)

fun smlFile (cwd:string, redefine:bool, group:groupInfo) (filename:string) =
  let val _= if redefine then () else echoFilename cwd filename
      val (imports, exports) = getConnections (cwd, redefine, filename)
  in
    Group.defineSource (group, modtime filename, redefine)
                       ("sml", filename, imports, exports)
  end handle Skip => NONE

fun crossRef (group:groupInfo) (otherFile:string) (filename:string) =
  case Group.lookupFile (hasher otherFile) of
     NONE => ()
   | (SOME (F {imports,...})) =>
       NameRefTable.append (!imports) fileH filename;

fun yaccFile (cwd:string, redefine:bool, group:groupInfo) (filename:string) =
  let val _ = if redefine then () else echoFilename cwd filename
      val imports = NameRefTable.create namespaceSize
      val exports = NameRefTable.create namespaceSize
  in
    NameRefTable.enter exports fileH [filename];
    crossRef group (filename ^ ".sml") filename;
    crossRef group (filename ^ ".sig") filename;
    Group.defineSource (group, modtime filename, redefine)
                       ("yacc", filename, imports, exports)
  end handle Skip => NONE

fun lexFile (cwd:string, redefine:bool, group:groupInfo) (filename:string) =
  let val _ = if redefine then () else echoFilename cwd filename
      val imports = NameRefTable.create namespaceSize
      val exports = NameRefTable.create namespaceSize
  in
    NameRefTable.enter exports fileH [filename];
    crossRef group (filename ^ ".sml") filename;
    Group.defineSource (group, modtime filename, redefine)
                       ("lex", filename, imports, exports)
  end handle Skip => NONE

fun analyzeFiles
      (cwd:string, redefine:bool, group:groupInfo, filelist:string list)
      :fileInfo option list =
  let val files = map (absoluteName cwd) filelist
      val (yaccFiles, lexFiles, smlFiles) = Auxiliary.arrangeFiles files
      val smlInfos  = map  (smlFile (cwd, redefine, group)) smlFiles
      val lexInfos  = map  (lexFile (cwd, redefine, group)) lexFiles
      val yaccInfos = map (yaccFile (cwd, redefine, group)) yaccFiles
  in smlInfos @ lexInfos @ yaccInfos end

fun undef (space, name, cwd, filename) =
  warn [space," ",name," undefined in ",relativeName cwd filename]

fun setFileDependencies (cwd:string)
      (group as G{main'groups,lib'groups,modtable,...} :groupInfo)
      (filename,_) (F {imports,depends,...}:fileInfo) =
  let fun defining'file (spaceH as (space,_)) (name, acc) =
        let val nameH = hasher name in
          case Group.lookupThruGroups group spaceH nameH of
             NONE =>
               if ModTable.lookup modtable (space, nameH) then acc
                 else (undef(space,name,cwd,filename); acc)
           | (SOME fref) =>
               if (filerefName fref) = filename then acc else fref::acc
        end
      val result = filerefNormalize
                     (NameRefTable.nestedFold (!imports) defining'file [])
  in
    depends := result
  end

fun computeDependencies (cwd:string) (mainGroup:groupInfo) =
  let fun doDependencies (group:groupInfo)
                         (isLibrary:bool) (_:unit) :unit =
        if isLibrary then ()
          else (Group.scanFiles group (setFileDependencies cwd mainGroup); ())
  in Group.groupsFold mainGroup false doDependencies (); ()
  end

type currency =
  {sourceName:string, sourceTime:time, envUpdateTime:time,
   targetTime:time, sourceModified:bool, envCurrent:bool, targetCurrent:bool}

fun newAs' (a:currency, b:currency) = newAs (#sourceTime a, #sourceTime b)

datatype 'a traversal = CIRCLE | OK of 'a

fun sourceTraversal
      (group:groupInfo) (first :string list) (firstOnly:bool)
      (operate :string -> fileInfo -> bool) (accum:bool) :bool =
  let val trav = Hash.createDefault ([]:(bool traversal) list)
      fun do'dependee ((isLibrary,sourceNameH as (sourceName,_)),accum) :bool =
            let val dependee =
                  case Hash.lookup trav sourceNameH of
                     (SOME CIRCLE) => false 
                   | (SOME (OK success)) => success
                   | NONE => do'source sourceNameH (lookupFile' sourceNameH)
            in dependee andalso accum end
      and do'source (sourceNameH as (sourceName,_))
                    (fileInfo as F {depends, toolH, env'current,
                                    target'current,...}:fileInfo) :bool =
          case Hash.lookup trav sourceNameH of
             (SOME CIRCLE) =>  false
           | (SOME (OK success)) => success
           | NONE => 
              let val _ = Hash.enter trav sourceNameH CIRCLE
                  val dependees = fold do'dependee (!depends) true
                  val success = dependees andalso (operate sourceName fileInfo)
              in
                Hash.enter trav sourceNameH (OK success);
                success
              end
      fun try'source (nameH as (name,_)) fileInfo accum =
        (do'source nameH fileInfo) andalso accum
      fun do'group (grp:groupInfo) (isLibrary:bool) accum :bool =
        if isLibrary then accum else Group.foldFiles grp try'source accum
      fun do'file (filename:string, accum:bool) :bool =
        let val filenameH = hasher filename
            val fileInfo = lookupFile' filenameH
        in (do'source filenameH fileInfo) andalso accum
        end
      val first'result = fold do'file (rev first) true
  in
    if firstOnly
      then first'result
      else Group.groupsFold group false do'group first'result
  end

fun currencyCheck
      (group as G{groupId,...}:groupInfo) :bool * (string list) =
  let val trav = Hash.createDefault ([]:(currency traversal) list)
      val trail :string list ref = ref []
      fun do'dependee ((isLibrary,sourceNameH as (sourceName,_)), accum) :currency =
            let val envUpdateTime'depender = (#envUpdateTime accum)
                val targetTime'depender = (#targetTime accum)
                val dependee =
                  case Hash.lookup trav sourceNameH of
                     (SOME CIRCLE) =>
                       (Auxiliary.circle group (sourceName::(!trail));
                        Util.err ["Giving up."])
                   | (SOME (OK currency)) => currency 
                   | NONE => 
                       do'source isLibrary sourceNameH
                                 (lookupFile' sourceNameH)
                val result =
                  {envUpdateTime = envUpdateTime'depender,
                   targetTime = targetTime'depender,
                   sourceTime = (#sourceTime accum),
                   sourceName = (#sourceName accum),
                   sourceModified = (#sourceModified accum),
                   envCurrent = ((#envCurrent accum) andalso
                     (#envCurrent dependee) andalso
                     (newAs(envUpdateTime'depender,#envUpdateTime dependee))),
                   targetCurrent = ((#targetCurrent accum) andalso
                     (#targetCurrent dependee) andalso
                     (newAs(targetTime'depender,#targetTime dependee)))}
            in
              result
            end
      and do'source isLibrary (sourceNameH as (sourceName,_))
                    (fileInfo as F {depends, toolH as (tool,_), envUpdateTime,
                                    envObjectTime, env'current, target'name, 
                                    target'current,...}:fileInfo) :currency =
          case Hash.lookup trav sourceNameH of
             (SOME CIRCLE) => (Auxiliary.circle group (sourceName::(!trail));
                               Util.err ["Giving up."])
           | (SOME (OK currency)) => currency
           | NONE => 
              let val _ = Hash.enter trav sourceNameH CIRCLE
                  val _ = trail := sourceName::(!trail)
                  val isSML = (tool = "sml")
                  val ToolInternals.Tool {targetNameOf, validTarget,...} =
                        ToolInternals.getToolInfo toolH
                  val sourceTime = modtime sourceName
                  val targetName = if isSML then (!target'name)
                                     else targetNameOf sourceName
                  val targetTime = modtime targetName
                  val sourceUnchanged = newAs (!envObjectTime, sourceTime)
                  val targetOK = (newAs (targetTime, sourceTime)) andalso
                                (validTarget (!groupId) sourceName targetName)
                  val start = {envUpdateTime=(!envUpdateTime),
                        targetTime=targetTime,
                        sourceTime=sourceTime, sourceName=sourceName,
                        sourceModified=not sourceUnchanged,
                        envCurrent=sourceUnchanged, targetCurrent=targetOK}
                  val currency = fold do'dependee (!depends) start
              in
                Hash.enter trav sourceNameH (OK currency);
                trail := tl (!trail);
                env'current := (#envCurrent currency);
                target'current := (#targetCurrent currency);
                currency
              end
      fun process'source (nameH as (name,_)) fileInfo accum =
        let val {envCurrent,...} = do'source false nameH fileInfo
        in accum andalso envCurrent end
      fun do'group (grp:groupInfo) (isLibrary:bool) accum =
        Group.foldFiles grp process'source accum
      fun appendModifiedSource (sourceName,_) currencyTraversal accum =
        case (currencyTraversal :currency traversal) of
           CIRCLE => accum
         | (OK (currency as {sourceModified,...})) =>
             if sourceModified then currency::accum else accum
      fun convert (currency as {sourceName,...}:currency) = sourceName
      val environmentCurrent = Group.groupsFold group false do'group true
      val modsources = Hash.fold trav appendModifiedSource []
      val modifiedSources = map convert (ListSort.sort newAs' modsources)
  in (environmentCurrent, modifiedSources) end

fun fileChanges (cwd:string, group:groupInfo) =
  let fun didChange (filenameH as (filename,_))
                    (info as F {infoTime,...}:fileInfo) acc =
        let val sourceTime = modtime filename in
          if Util.isZeroTime sourceTime then filename::acc
            else if Util.newer (sourceTime, !infoTime)
                   then filename::acc else acc
        end
  in
   analyzeFiles (cwd, true, group, Group.foldFiles group didChange [])
  end

fun groupInfoChanges
      (group as G{infoTime=groupInfoTime,...}:groupInfo)
      (filenameH as (filename,_))
      (info as F {infoTime,...}:fileInfo) changed =
  if Util.newer (!infoTime, !groupInfoTime)
    then (SOME info)::changed else changed

fun updateDefinitions (cwd:string)(group as (G {groupId,...}) :groupInfo)
                      (isLibrary:bool)(acc:unit) =
    (case Group.connFile group of
        NONE =>
          (fileChanges (cwd, group);
           Group.updateNamespaces group
             (Group.foldFiles group (groupInfoChanges group) []))
      | (SOME connFile) =>
          let val defTime = modtime connFile
              val dir = Pathname.directoryPart connFile in
            if Util.newer (defTime, Group.infoTime group)
              then
                let val connections = Connections.get connFile
                    val _ = Group.clearFiles group
                    val infoList =
                          map (Group.defineSource (group, defTime, true))
                              connections
                in
                  Group.createNamespaces false group infoList
                end
              else ()
          end)

fun processSource
      (fileAction :sourceInfo -> unit)
      (group as G {groupId,...} :groupInfo)
      (cwd :string) (sourcePathname :string)
      (F {depends, env'current, target'current, target'name, index,
          toolH as (tool,_), infoTime,...}:fileInfo) :bool =
 let val compiler'indexing = !System.Control.indexing
     fun reset () = (System.Control.indexing := compiler'indexing)
 in
  if tool = "" then true else
    let val sourceName = relativeName cwd sourcePathname
        val isSML = (tool = "sml")
        val ToolInternals.Tool
              {targetNameOf,loadSource,genTarget,loadTarget,checkLoad,
               compileSource,validTarget,...} = ToolInternals.getToolInfo toolH
        val targetName = relativeName cwd
                  (if isSML then (!target'name) else targetNameOf sourceName)
        val g = !groupId
        fun updateSource () =
          Group.updateEnv (g, sourcePathname, targetName, true)
        fun updateTarget () =
          Group.updateEnv (g, sourcePathname, targetName, false)
        val updateCheckLoad =
          if validTarget g sourceName targetName then updateSource
            else updateTarget
        val importedFiles = map filerefName (!depends)
        fun doAction () =
          fileAction (Source
           {sourceName = sourceName, targetName = targetName, group = !groupId,
            envCurrent = (!env'current), targetCurrent = (!target'current),
            toolName = tool, dependsOn = importedFiles,
            loadSource = fn()=>
              (loadSource g sourceName targetName; updateSource()),
            genTarget = fn()=> genTarget g sourceName targetName,
            loadTarget = fn()=> 
              (loadTarget g sourceName targetName; updateTarget()),
            compileSource=fn()=>
              (compileSource g sourceName targetName; updateSource()),
            checkLoad = fn()=>
              (checkLoad g sourceName targetName; updateCheckLoad())})
    in
      if isSML then System.Control.indexing := (!index) else ();
      Interrupt.handleInterrupt doAction;
      true
    end handle
           Interrupt => (reset(); raise Interrupt)
         | (System.Compile.Compile msg) =>
             (reset();
              say ("\n? Exception Compile raised: "^msg^"\n");
              if (!continueAfterError)
                then (say "\n  Make continued...\n"; false)
                else (say "\n? errors were found during make.\n";
                      raise (System.Compile.Compile msg)))
         | any =>
             (reset(); 
              if (!continueAfterError)
                then 
                  (say ("\n? Exception raised: "^(System.exn_name any)^
                          "\n  Make continued...\n");
                   false)
                else (say "\n? errors were found during make.\n";
                      raise any))
 end

fun makeGroup (libraryLevel:int) (fileAction :sourceInfo -> unit)
              (group as G{initialized,...} :groupInfo) :int =
  let val maxLibraryTime = 
            Group.groupsFold group false (doLibraries libraryLevel) 0
  in
    if (libraryLevel = 0) orelse
       ((!initialized <= maxLibraryTime) andalso (!makeLibraries))
      then
        let val cwd = getwd ()
            val _ = Group.groupsFold group false (updateDefinitions cwd) ()
            val _ = computeDependencies cwd group
            val (_, changes) = currencyCheck group
            val doWork = processSource fileAction group cwd
            val result = sourceTraversal group changes false doWork true
        in
          if result
            then let val (environCurrent,_) = currencyCheck group
                 in initialized :=
                      (if environCurrent
                            then Util.seconds(Util.currentTime ()) else 0)
                 end
            else 
              (say "\n? Errors were found during make.\n";
               initialized := 0;
               raise CompilingError);
          (!initialized)
        end
      else
        (!initialized)
  end

and doLibraries (libraryLevel:int) (group:groupInfo)
                (isLibrary:bool) (accum:int) :int =
  if isLibrary then accum
    else fold (makeLibrary libraryLevel) (rev(Group.libraryGroups group)) accum

and makeLibrary (libraryLevel:int) (group:groupInfo, accum:int) :int =
  let val thisLibraryTime = 
            makeGroup (libraryLevel+1) (!libraryAction) group
  in
    if thisLibraryTime > accum then thisLibraryTime else accum
  end

fun makeAll (fileAction :sourceInfo -> unit) (groupId:group) :unit =
  (makeGroup 0 fileAction (Group.findGroup groupId); ())

val structureIndex = 0
val functorIndex   = 1
val signatureIndex = 2
val hashKeyTable =
  let val table = Array.array (3, ("",0)) in
    Array.update (table, structureIndex, structureH);
    Array.update (table, functorIndex, functorH);
    Array.update (table, signatureIndex, signatureH);
    table
  end
fun hashKey x = Array.sub (hashKeyTable, x)

fun get'demanded'files (group:groupInfo) (demand:string) :string list =
  let val demand'list = rev (StringXtra.breakAtBlanks demand)
      val modules = Array.array (3, []:string list)
      fun arrange (word'list:string list, moduleIndex:int) =
        case word'list of
           [] => ()
         | (head::tail) =>
             (case head of
                 "structure" => arrange (tail, structureIndex)
               | "functor"   => arrange (tail, functorIndex)
               | "signature" => arrange (tail, signatureIndex)
               | name =>
                   (Array.update (modules, moduleIndex,
                                  name::(Array.sub (modules, moduleIndex)));
                    arrange (tail, moduleIndex)))
      fun get'filename (spaceH as (space,_)) name =
        case Group.lookupThruGroups group spaceH (Hasher.hasher name) of
           NONE => (warn [space," ",name," undefined"]; "")
         | (SOME (_, (filename,_))) => filename
      fun get'files (moduleIndex, accum) =
        accum @ (map (get'filename (hashKey moduleIndex))
                     (Array.sub (modules, moduleIndex)))
      fun removeEmpty (name, accum) = if name = "" then accum else name::accum
  in
    arrange (demand'list, structureIndex);
    fold removeEmpty (fold get'files [0,1,2] []) []
  end

fun nameModtime name = (modtime name, name)
fun nameOnly (_,name) = name
fun newer ((t1,n1),(t2,n2)) = Util.newer (t1, t2)

fun makeOnDemand action (groupId:group) (demand:string) :unit =
  let val cwd = getwd ()
      val group = Group.findGroup groupId
      val _ = Group.groupsFold group false (updateDefinitions cwd) ()
      val _ = (computeDependencies cwd group; currencyCheck group)
      val demanded'files = map nameModtime (get'demanded'files group demand)
      val filelist = map nameOnly (ListSort.sort newer demanded'files)
      val doWork = processSource action group cwd
      val result = sourceTraversal group filelist true doWork true
  in
    if result then () else
      (say "\n? Errors were found during make.\n"; raise CompilingError)
  end

val make = makeOnDemand doCompile

fun makeWhat (g:group) (demand:string) =
  let val libAction = !libraryAction
      fun reset () = (libraryAction := libAction)
  in
    libraryAction := doPrint;
    (makeOnDemand doPrint g demand) handle any => (reset(); raise any);
    reset()
  end

fun dependsOn (sourceName:string) :string list =
  let val (F{depends,...}) = lookupFile' (hasher sourceName)
      val cwd = getwd()
      val rel = relativeName cwd
      fun nameOf x = rel (filerefName x)
  in map nameOf (!depends) end

val connections  = Auxiliary.connections
val connections' = Auxiliary.connections'

end
