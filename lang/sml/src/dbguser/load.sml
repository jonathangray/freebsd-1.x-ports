(* Load user-level debugger code. *)
use "dbguser/util.sml";
use "dbguser/interface.sml";
use "dbguser/breaks.sml";
use "dbguser/emacs.sml";
use "dbguser/commands.sml";
open UserDebugCommands;
(* This isn't quite right, since we want only the structure 
    UserDebugCommands and its contents (and perhaps UserDebugInterface)
    to be visible at this point.  *)
(* Put contents of UserDebugCommands into ZdebugCommandsEnv.  This
    allows commands to be seen before attempting a lookup in the debugger
    emulated environment, which is slow and bound to fail. 
    There may be cleaner ways to accomplish this... *)
UserDebugInterface.ZdebugCommandsEnv := System.Compile.eval_stream(
		       open_string "open UserDebugCommands",
		       System.Env.layerEnv(!System.Env.topLevelEnvRef,
					   !System.Env.pervasiveEnvRef));

(* Compile debugger versions of pervasives *) 
(* following is to get reasonable definitions for libraries. *)
let open System.Control.CG
in (bodysize := 40;
    rounds := 3;
    reducemore := 15)
end;
use "dbguser/hsignals.sml";
use "dbguser/hio.sml";
use "dbguser/hstore.sml";
(* Pre-define debugPervEnv to be standard pervasives in order to compile
list and general. *)
UserDebugInterface.ZdebugPervEnv := !System.Env.pervasiveEnvRef;
UserDebugCommands.usedbg "dbguser/list.sml"; 
UserDebugCommands.usedbg "dbguser/general.sml";  
use "dbguser/debugperv.sml";
(* Again, this isn't quite right, since we'd like the debugger versions of
the pervasives to disappear now... *)
(* Set the debugger pervasive environment.  Again, there may be a cleaner
   way to do this... *)
let open System.Env System.Compile 
    val e0 = layerEnv(!topLevelEnvRef ,!pervasiveEnvRef)
in UserDebugInterface.ZdebugPervEnv := 
              layerEnv(eval_stream(open_string "open DEBUG_PERV", e0),
		       e0)
end				   

