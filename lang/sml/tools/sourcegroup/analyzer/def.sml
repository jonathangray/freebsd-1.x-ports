(* Copyright (c) 1992 by Carnegie Mellon University *)

local open SourceGroup SourceAction FileList in
  val _ = SMLTool.targetNamer := sysBinary
  val createGroup = createInEnv (!Compile.compilingEnv)

  val g = createGroup [Sources ["pervasives.sml",
     "moduleNames.sig","moduleDecls.sig","traverse.sig","parse.sig",
     "moduleNames.sml","moduleDecls.sml","traverse.sml","parse.sml",
     "analyze.sml", "analyze.sig", "link.sml", "process.sig",
     "scopes.sml", "scopes.sig", "process.sml"]]
  fun m () =
    (System.Control.Print.signatures := 0;
     SourceGroup.make g "Analyze"
       handle any => (System.Control.Print.signatures := 2; raise any);
     System.Control.Print.signatures := 2;
     Compile.openEnv())

end
