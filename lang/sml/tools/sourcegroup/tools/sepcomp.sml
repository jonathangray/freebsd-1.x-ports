(* Copyright (c) 1992 by Carnegie Mellon University *)

signature SEPCOMP = sig
 val deleteTargets  :bool ref
     (* If true, then before writing a new target file, delete the old one.
        If not true, then write target file without deleting the old one.
        By deleting targets before writing the new one, a symbolic link
        can be replaced without effecting the old target. *)

 val printFilenames :bool ref
     (* If true, then when a file is opened print "reading...", and when
        a file is written print "writing...".  If false, be silent. *)

 val makeObjectFile :string * string * staticEnv -> staticEnv
     (* makeObjectFile(src,target,senv) ==> senv'
      *  compiles the source file src to produce a compUnit that
      *  is written to file target.  The result senv' is the incremental
      *  static environment produced by compilation *)

 val compileFile :string * string * environment -> environment
     (* compileFile(src,target,env) ==> env'
      *  similar to makeObjectFile but also executes compiled compUnit
      *  and produces full incremental environment env' *)

 val loadSourceFile :string * environment -> environment
     (* loadSourceFile(src,env) ==> env'
      *  compiles and executes source file src to produce incremental
      *  full environment env', but does not write out compUnit *)

 val loadObjectFile :string * environment -> environment
     (* loadObjectFile(target,env) ==> env'
      *  reads compUnit from file target and executes it to produce
      *  incremental environment env' *)

 val validObjectFile :string -> bool
     (* checks to see if its argument designates a valid target file
      *  for the current system version and architecture. *)

 val openEnv :environment -> unit
     (* adds the environment to the toplevel environment. *)
end


structure SepComp :SEPCOMP = struct

val deleteTargets  = ref true
val printFilenames = ref true

open System.Env System.Compile

val say = System.Print.say

fun reading file =
  if !printFilenames then say ("[reading " ^ file ^ "]\n") else ()
fun writing file = 
  if !printFilenames then say ("[writing " ^ file ^ "]\n") else ()
fun closing file =
  if !printFilenames then say ("[closing " ^ file ^ "]\n") else ()

fun withSource (sourceName:string)
      (action :source -> 'a -> 'b) (argument:'a) :'b =
  let val sourceStream = open_in sourceName
      val source = makeSource (sourceName, 1, sourceStream, false, 
			       {linewidth= !System.Print.linewidth,
				flush=System.Print.flush,
				consumer=System.Print.say})
      fun cleanup () = (closeSource source; closing sourceName)
      val result = action source argument
                     handle exn => (cleanup(); raise exn)
  in cleanup(); result end

(* Version stamp included in object files. Specifies compiler version,
   machine type (= !System.architecture), and runtime stamp. *)

val targetVersion =
  System.version^(!System.architecture)^(!System.runtimeStamp)^"\n"

fun targetRead (s:instream) :System.Compile.compUnit =
  System.Unsafe.blast_read (s, can_input s)

val gcmessages = System.Control.Runtime.gcmessages 

val targetWrite' :outstream * compUnit -> int = System.Unsafe.blast_write

fun targetWrite (stream, obj) =
  let val oldmsgs = !gcmessages in
     gcmessages := 0;
     (targetWrite'(stream, obj); gcmessages := oldmsgs)
       handle e => (gcmessages := oldmsgs; raise e)
  end

fun readCompUnit (targetName:string) :compUnit =
  let fun reader target () =
        if (input_line target) <> targetVersion
          then
            (say ("? target file " ^ targetName ^
                  " is the wrong format; quitting\n");
             raise (Compile "wrong target format"))
          else targetRead target 
  in
    IO_Stream.withInStream (open_in targetName) reader ()
  end

fun deleteFile (filename:string) =
  (System.Unsafe.SysIO.unlink filename) handle _ => ()

fun ignoreWriteError targetName =
  (deleteFile targetName;  (* remove half-baked target *)
   say "% writing target file failed, ignored; compilation continued.\n")

fun writeCompUnit (compiledUnit:compUnit, targetName:string) =
  let fun writer target () =
        (outputc target targetVersion;
         targetWrite (target,compiledUnit))
  in
    Interrupt.handleInterrupt
     (if !deleteTargets then deleteFile targetName else ();
      IO_Stream.withOutStream (open_out targetName) writer)
  end
    handle
       Io msg =>
        (say ("\n" ^ msg ^ "\n");
         ignoreWriteError targetName)
     | Interrupt =>
        (say "\n% Interrupt encountered.\n";
         ignoreWriteError targetName;
         raise Interrupt)
     | any =>
        (say ("% Exception " ^ (System.exn_name any) ^ " raised.\n");
         ignoreWriteError targetName)

fun compileSource (sourceName:string, senv:staticEnv) :compUnit =
  let fun comp source () = compile (source, senv) in
    withSource sourceName comp ()
  end

fun compileFile (sourceName:string, targetName:string, env:environment) =
  let val _ = reading sourceName
      val compUnit = compileSource(sourceName, staticPart env)
  in
    writing targetName;
    writeCompUnit(compUnit,targetName);
    execute(compUnit, env)
  end

fun makeObjectFile (sourceName:string, targetName:string, senv:staticEnv) =
  let val _ = reading sourceName
      val compUnit as ({staticEnv,...},_) = compileSource(sourceName,senv)
  in
    writing targetName;
    writeCompUnit(compUnit,targetName);
    staticEnv
  end

fun loadSourceFile (sourceName:string, env:environment) :environment =
  (reading sourceName;
   execute(compileSource(sourceName,staticPart env),env))

fun loadObjectFile (targetName:string, env:environment) :environment =
  let val _ = reading targetName
      val (static,code) = readCompUnit targetName in
    execute((changeLvars static,code),env)
  end

fun validObjectFile (filename:string) :bool =
  let fun checker stream () = ((input_line stream) = targetVersion)
  in
    IO_Stream.withInStream (open_in filename) checker ()
  end handle _ => false

fun openEnv (env:environment) :unit =
  topLevelEnvRef := concatEnv(env, !topLevelEnvRef)

end
