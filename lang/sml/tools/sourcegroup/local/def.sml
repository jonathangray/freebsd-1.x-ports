(* Copyright (c) 1992 by Carnegie Mellon University *)

val _ = (SMLTool.targetNamer := SourceAction.sysBinary)

val libFiles = FileList.inFile ["sources.txt"]

val libGroup = SourceGroup.create [SourceGroup.Sources libFiles]

fun mkLib () = SourceGroup.Expert.makeAll SourceAction.doCompile libGroup
