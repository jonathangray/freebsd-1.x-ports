(* shareglue.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * shareglue: a functor application common to all architectures.
 *)

functor IntShare(structure Machm: CODEGENERATOR
		 functor Debugger : DEBUGGER
		 val fileExtension: string) =
struct
  structure Comp = CompileUnit(structure Machm = Machm)

  structure Inter = Interact(structure Machm = Machm)

  structure Foo = InitHooks(structure Interact = Inter
			    structure Compile = Comp)

  (* initializations *)
  val _ =
    (Environment.pervasiveEnvRef := BootEnv.makePervEnv();
     Environment.topLevelEnvRef := Environment.emptyEnv;
     System.architecture := fileExtension)

  structure DebugFoo = Debugger(structure Machm = Machm)

  (* launch interactive loop *)
  val _ =
    (System.Print.say "Go for it\n";
     Inter.interact())

end



