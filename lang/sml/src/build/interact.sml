signature INTERACT =
sig
  val interact : unit -> unit
  val use_file : string -> unit
  val use_stream : instream -> unit
  val eval_stream : instream * Environment.environment -> 
                       Environment.environment
end

functor Interact(structure Machm : CODEGENERATOR) : INTERACT =
struct
  fun stdParams() = {baseEnv= !Environment.pervasiveEnvRef,
		     localEnvRef=Environment.topLevelEnvRef,
		     parser=Elaborate.parse (fn dec => dec),
		     generate=Machm.generate,
		     perform=(fn exec => exec()),
		     isolate=CompUtil.isolate,
		     printer=PPDec.ppDec}

  fun interact() =  EvalLoop.interact (stdParams())
  fun use_file (fname: string) =
      (app System.Print.say ["[opening ",fname,"]\n"];
       EvalLoop.eval_stream (stdParams())
		  (fname,(open_in fname
			  handle Io s =>
			      (app System.Print.say["[use failed: ",s,"]\n"];
			       raise ErrorMsg.Error))))

  fun use_stream (stream: instream) =
      EvalLoop.eval_stream (stdParams()) ("<instream>", stream)

  fun eval_stream (stream: instream, baseEnv: Environment.environment) : 
      Environment.environment =
      let val localEnvRef = ref Environment.emptyEnv
       in EvalLoop.eval_stream
	          {baseEnv=baseEnv,
		   localEnvRef=localEnvRef,
		   parser=Elaborate.parse (fn dec => dec),
		   generate=Machm.generate,
		   perform=(fn exec => exec()),
		   isolate=CompUtil.isolate,
		   printer=PPDec.ppDec}
		  ("<instream>", stream);
	  !localEnvRef
      end

end (* functor Interact *)




