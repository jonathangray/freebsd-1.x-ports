(* Copyright (c) 1992 by Carnegie Mellon University *)

signature COMPILE =
  sig
    val deleteTargets  :bool ref
    val printFilenames :bool ref
    val compile :string * string -> unit
    val genTarget :string * string -> unit
    val loadTarget :string -> unit
    val loadSource :string -> unit
    val validTarget :string -> bool
    val openEnv :unit -> unit
    val resetEnv :unit -> unit
    val compilingEnv :environment ref
  end

structure Compile :COMPILE = struct

val deleteTargets  = SepComp.deleteTargets
val printFilenames = SepComp.printFilenames

val compilingEnv :environment ref = ref (System.Env.emptyEnv ())

fun openEnv () = SepComp.openEnv (!compilingEnv)

fun resetEnv () = compilingEnv := System.Env.emptyEnv ()

fun start () = System.Env.layerEnv (!compilingEnv,!System.Env.pervasiveEnvRef)

fun loadSource sourceName =
  compilingEnv :=
    System.Env.concatEnv
      (SepComp.loadSourceFile (sourceName, start()),
       !compilingEnv)

fun genTarget (sourceName, targetName) =
  (SepComp.makeObjectFile
     (sourceName, targetName, System.Env.staticPart (start()));
   ())

fun loadTarget targetName =
  compilingEnv :=
    System.Env.concatEnv
      (SepComp.loadObjectFile (targetName, start()),
       !compilingEnv)

fun compile (sourceName, targetName) =
  compilingEnv :=
    System.Env.concatEnv
      (SepComp.compileFile (sourceName, targetName, start()),
       !compilingEnv)

fun validTarget targetName =
  SepComp.validObjectFile targetName

end
