(* Copyright (c) 1992 by Carnegie Mellon University *)

functor LexToolDefFun () :TOOLDEF = struct

val toolName = "lex"
val processor = ref "sml-lex"
val targetNamer = ref (fn (pathname:string) => (pathname ^ ".sml"))

fun runLex (source:string) = 
  let val callString = ((!processor)^" "^source)
      val _ = (print ("[" ^ callString ^ "]\n"))
      val status = System.system callString
  in if (status = 0) then ()
       else raise SourceGroup.CompilingError;
   print "[ending   sml-lex "; print source; print "]\n"
  end

fun fileExists filename =
  let val _ = System.Unsafe.SysIO.mtime (System.Unsafe.SysIO.PATH filename) in
    true
  end handle _ => false

fun loadSource (group:SourceGroup.group) (source:string) (target:string) =
  runLex source;
fun genTarget (group:SourceGroup.group) (source:string) (target:string) =
  runLex source;
fun loadTarget (group:SourceGroup.group) (source:string) (target:string) = ()
fun validTarget (group:SourceGroup.group) (source:string) (target:string) = 
  fileExists target
fun compileSource (group:SourceGroup.group) (source:string) (target:string) =
  runLex source;
fun checkLoad (group:SourceGroup.group) (source:string) (target:string) =
  if validTarget group source target
    then loadTarget group source target
    else loadSource group source target
end;
