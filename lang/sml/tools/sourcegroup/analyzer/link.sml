(* Copyright (c) 1992 by Carnegie Mellon University *)

structure ModuleDecls = ModuleDeclsFun (ModuleNames)
structure Traverse = TraverseFun (ModuleDecls)
structure Scopes = ScopesFun (structure ModuleDecls = ModuleDecls)

structure Process = ProcessFun
  (structure ModuleDecls = ModuleDecls
   structure Scopes = Scopes)

structure Analyze = AnalyzeFun
  (structure Traverse = Traverse 
   structure Process = Process
   structure Scopes = Scopes
   structure Parse = Parse)
