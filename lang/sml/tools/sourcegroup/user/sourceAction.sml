(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

structure SourceAction :SOURCEACTION = struct

open SourceGroup SourceGroup.Expert

val withOutStream = IO_Stream.withOutStream
val mergePathnames = Pathname.mergePathnames

fun printSep ([] :string list) (sep :string) :unit = ()
  | printSep (a::[]) sep = print a
  | printSep (a::(rest as (b::c))) sep =
      (print a; print sep; printSep rest sep)

fun fileExists filename =
  let val _ = System.Unsafe.SysIO.mtime(System.Unsafe.SysIO.PATH filename)
  in true end handle _ => false

fun appendBin pathname = pathname ^ ".bin"

fun replaceExtension pathname = (Pathname.stripExtension pathname) ^ ".bin"

fun systemBinary system pathname =
  let val (dirname, filename) = Pathname.splitDirFile pathname
      val sysDirName = Pathname.mergeDirFile dirname system
  in
    if fileExists sysDirName then 0 else System.system ("mkdir " ^ sysDirName);
    mergePathnames [sysDirName, filename^".bin"]
  end

val sysBinary = systemBinary ".@sys"

val architectureBinary = systemBinary (!System.architecture)

fun doClean (Source {targetName,...} :sourceInfo) =
  ((System.Unsafe.SysIO.unlink targetName;
    printSep ["rm ", targetName, "\n"] "")
      handle (Io s) => () | SystemCall => ())

fun doLoadSource
      (Source{envCurrent,loadSource,...}:sourceInfo) =
  if envCurrent then () else loadSource()

fun doCompile (Source {envCurrent, targetCurrent, loadTarget,
                       compileSource,...}:sourceInfo) =
 if targetCurrent
   then if envCurrent then () else loadTarget ()
   else compileSource ()

fun doForceCompile (Source {compileSource,...}:sourceInfo) = compileSource ()

fun doLoadLibrary (Source {envCurrent, targetCurrent, loadSource,
                           loadTarget,...}:sourceInfo) =
  if envCurrent then ()
    else if targetCurrent then loadTarget() else loadSource()

fun doNothing (_:sourceInfo) = ()

fun doPrint (Source {sourceName, targetName, toolName,
                     envCurrent, targetCurrent,...} :sourceInfo) =
  if targetCurrent
    then if envCurrent then () else
      (case toolName of
          "yacc" => () | "lex" => ()
        | _ => (print "loadTarget \""; print targetName; print "\";\n"))
    else (print "compile \""; print sourceName; print "\";\n")

fun doShowUse (pr:string->unit)(Source {sourceName,toolName,...} :sourceInfo) =
 case toolName of
    "sml" => (pr "use \""; pr sourceName; pr "\";\n")
  | "lex" => ()
  | "yacc" => ()
  | _ => (pr "(* load "; pr toolName; pr " "; pr sourceName; pr " *)\n")

fun showUses' out (group:group) =
  makeAll (doShowUse (outputc out)) group

fun showUses filename (group:group) =
  withOutStream (open_out filename) showUses' group

fun showDemandedUses' out (group:group, demand:string) =
  makeOnDemand (doShowUse (outputc out)) group demand

fun showDemandedUses filename (group:group, demand:string) =
  withOutStream (open_out filename) showDemandedUses' (group, demand)

fun prJustified (pr:string->unit) (lineSize:int)
                (indent:string) ([]:string list) (sep:string) = ()
  | prJustified pr (lineSize:int)(indent:string)(a::[]) sep =
      (if (lineSize+(String.size a)) >= 80 then pr "\n  " else ();
       pr a)
  | prJustified pr (lineSize:int)(indent:string)(a::(rest as (b::c))) sep =
      let val textSize = (String.size a) + (String.size sep)
          val lineLength =
                if (lineSize+textSize) >= 80
                  then (pr "\n"; pr indent; size indent) 
                  else lineSize
      in
        pr a; pr sep;
        prJustified pr (lineLength+textSize) indent rest sep
      end

fun showDependencies' out (group:group) =
  let val pr = outputc out
      fun doDepends
            (Source
               {sourceName, toolName ,dependsOn,...}:sourceInfo) =
        (pr sourceName; pr ":";
         prJustified pr 80 "  " dependsOn " "; pr "\n")
  in
    makeAll doDepends group
  end

fun showDependencies filename (group:group) =
  withOutStream (open_out filename) showDependencies' group

end
