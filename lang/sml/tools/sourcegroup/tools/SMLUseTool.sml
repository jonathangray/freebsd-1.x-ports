(* Copyright (c) 1992 by Carnegie Mellon University *)

functor SMLUseToolDefFun () :TOOLDEF = struct
 val toolName = "sml"
 val processor = ref ""
 val targetNamer = ref (fn (pathname:string) =>  (pathname^".bin"))
 fun err (msg:string) =
  (print ("? SML Tool: Operation " ^ msg ^
           " not available in this SML compiler\n");
   raise SourceGroup.CompilingError)
 fun loadSource (group:SourceGroup.group) (source:string) (target:string) =
   IO.use source
 fun genTarget (group:SourceGroup.group) (source:string) (target:string) =
   err "genTarget"
 fun loadTarget (group:SourceGroup.group) (source:string) (target:string) =
   err "loadTarget"
 fun compileSource (group:SourceGroup.group) (source:string) (target:string) =
   err "compileSource"
 fun checkLoad (group:SourceGroup.group) (source:string) (target:string) =
   loadSource group source target
 fun validTarget (group:SourceGroup.group) (source:string) (target:string) =
   false
end;
