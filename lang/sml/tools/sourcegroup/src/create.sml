(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor SourceGroupCreateFun
  (structure SourceGroupCore :SOURCE_GROUP_CORE
   structure DirFile :DIRFILE
   structure IO_Stream :IO_STREAM
   structure Pathname :PATHNAME
  ) :SOURCE_GROUP_CREATE = struct

 structure SourceGroupCore = SourceGroupCore
 structure Group = SourceGroupCore.Group
 structure Data = Group.Data
 structure Connections = SourceGroupCore.Connections
 structure Util = SourceGroupCore.Util
 structure Args = SourceGroupCore.Args

open Data

exception Quit
exception Skip

val say = System.Print.say
val analyzeFiles = SourceGroupCore.analyzeFiles
val absoluteName = Pathname.absoluteName
val relativeName = Pathname.relativeName
val getwd = Pathname.getwd
val modtime = DirFile.timeModified

fun defineAuto (cwd:string, filelist :string list) =
  let val group :groupInfo = Group.newSubGroup NONE in
    Group.createNamespaces true group (analyzeFiles(cwd,false,group,filelist));
    group
  end

fun translateFilename (cwd:string) (t, filename, imp, exp) =
  (t, absoluteName cwd filename, imp, exp)

fun defineManual (cwd:string)
      ((connFile :string), (acc :groupInfo list)) :groupInfo list =
  let val fullConnFilename = absoluteName cwd connFile
      val rel = relativeName cwd
      val _ = if !SourceGroupCore.quietCreation then () else
                say ("[Connections " ^ (rel fullConnFilename) ^ "]\n")
      val group :groupInfo = Group.newSubGroup (SOME fullConnFilename)
      val defTime = modtime fullConnFilename
      val dir = Pathname.directoryPart fullConnFilename
      val connections = map (translateFilename dir)
                            (Connections.get fullConnFilename)
      val infoList = map (Group.defineSource (group,defTime,false)) connections
  in
    Group.createNamespaces true group infoList;
    group::acc
  end

fun createGroup (modtable, args:Data.groupDescription list) :Data.group =
  let val cwd = getwd()
      val (sourceFiles, subgroups, connFiles) = Args.rearrange args
      fun pathlist (dir:string, line:string) =
        map (absoluteName dir) (StringXtra.breakAtBlanks line)
      fun work (dir:string)(instream:instream)(init:string list) :string list =
        let fun action acc =
              if end_of_stream instream then acc
                else action (acc @ (pathlist (dir, input_line instream)))
        in action init end
      val autoPart = defineAuto (cwd, sourceFiles)
      val newPart = fold (defineManual cwd) connFiles [autoPart]
      val mainGroup :groupInfo =
        Group.newMainGroup modtable (subgroups @ newPart) []
  in
    SourceGroupCore.computeDependencies cwd mainGroup;
    SourceGroupCore.currencyCheck mainGroup;
    Group.groupId mainGroup
  end

fun createInEnv(env:environment)(args:Data.groupDescription list) :Data.group =
  createGroup (Data.ModTable.create env, args)

fun create (args:Data.groupDescription list) :Data.group =
  createGroup (Data.ModTable.pervasives, args)

end
