(* Copyright (c) 1992 by Carnegie Mellon University *)

functor YaccToolDefFun () :TOOLDEF = struct

val toolName = "yacc"
val targetNamer = ref (fn (pathname:string) => (pathname ^ ".sml"))
val processor = ref "sml-yacc"

fun sigFile sourceName =
  let val name = (!targetNamer) sourceName in
    (substring (name, 0, (size name)-4)) ^ ".sig"
  end

fun runYacc (source:string) = 
  let val callString = ((!processor)^" "^source)
      val _ = (print ("[" ^ callString ^ "]\n"))
      val status = System.system callString
  in if (status = 0) then ()
       else raise SourceGroup.CompilingError;
   print "[ending   sml-yacc "; print source; print "]\n"
  end

fun fileExists filename =
  let val _ = System.Unsafe.SysIO.mtime (System.Unsafe.SysIO.PATH filename) in
    true
  end handle _ => false

fun loadSource (group:SourceGroup.group) (source:string) (target:string) =
  runYacc source;
fun genTarget (group:SourceGroup.group) (source:string) (target:string) =
  runYacc source;
fun loadTarget (group:SourceGroup.group) (source:string) (target:string) = ()
fun compileSource (group:SourceGroup.group) (source:string) (target:string) =
  runYacc source;
fun validTarget (group:SourceGroup.group) (source:string) (target:string) = 
  (fileExists target) andalso (fileExists (sigFile source))
fun checkLoad (group:SourceGroup.group) (source:string) (target:string) =
  if validTarget group source target
    then loadTarget group source target
    else loadSource group source target
end;
