(* Copyright 1992 by AT&T Bell Laboratories *)

signature BOOTENV =
sig
  val bootEnv : (Environment.staticEnv -> string -> Environment.staticEnv)
                 -> Environment.staticEnv * (int * int * int)
  val makePervEnv : unit -> Environment.environment
end

structure BootEnv: BOOTENV =
struct

  open ErrorMsg Modules ModuleUtil Access

  fun load env fname = (app System.Print.say ["[Loading ", fname, "]\n"]; 
                        ProcessFile.process(env,fname,NONE))

 (* initializing static environment *)

  fun bootEnv (loader: Environment.staticEnv -> string 
                       -> Environment.staticEnv) =
      let val err = fn _ => impossible "bootEnv"
	  val sigSymbols =
	     map Symbol.sigSymbol ["REF","LIST","ARRAY","BYTEARRAY","IO",
                                   "BOOL","STRING","INTEGER","REAL",
				   "VECTOR","GENERAL"]
	  val NJsymbol = Symbol.strSymbol "NewJersey"
	  val signatures = !System.Print.signatures
	  val _ = System.Print.signatures := 0
	  val _ = CoreInfo.resetCore();
          val env = {static=Prim.primEnv,inverse=InverseEnv.empty}
	  val env = load env "boot/assembly.sig"
	  val env = loader env "boot/core.sml"
	  val env = load env "boot/dummy.sml"
	  val markabs = !System.Control.markabsyn
			 before System.Control.markabsyn := false
	  val svCore as STRvar{access=Access.PATH[lvCore],...} =
		lookSTR(#static(env),[Symbol.strSymbol "Core"],err)
	  val _ = CoreInfo.setCore(#static(env),[Symbol.strSymbol "Core"])
	  val env = load env "boot/perv.sig"
	  val env = load env "boot/system.sig"
	  val env = loader env "boot/math.sml"
	  val env = loader env "boot/perv.sml"
	  val env = load env "boot/overloads.sml"
	  val _ = (System.Print.signatures := signatures)
	  val _ = (System.Control.markabsyn := markabs)
	  val mathSymb = Symbol.strSymbol "Math" and
	      initSymb = Symbol.strSymbol "Initial" and
	      ovldSymb = Symbol.strSymbol "Overloads"
          val senv = #static(env)
	  val STRvar{access=Access.PATH[lvMath],...} =
		       lookSTR (senv,[mathSymb],err)
	  and svInitial as STRvar{access=PATH[lvInitial],
				   binding=strInitial,...} =
		       lookSTR (senv,[initSymb],err)
	  and overLoads = lookSTR (senv,[ovldSymb],err)
	  val senv' = openStructureVar 
			(openStructureVar(Prim.fixOpEnv,svInitial), overLoads)
	  val senv' = fold (fn (name,e) => 
                               Env.bind(name,Env.look(senv,name),e))
		       sigSymbols senv'
	  val senv' = Env.bind
			  (NJsymbol, 
			   STRbind(STRvar{name=NJsymbol,access=PATH[lvInitial],
					  binding=strInitial}),
			   senv')
          val env' = Environment.makeStaticEnv(senv')
      in (env',(lvCore,lvInitial,lvMath))
      end

  fun makePervEnv () =
      let val ({static=pervStatEnv,inverse=pervInvEnv},(vCore,vInitial,vMath))
                 = bootEnv load
          (* bind runtime boot structures, Core, Math, and Initial, 
             in pervDynEnv *)
	  val {core,math,initial} = !System.Unsafe.pstruct
	  val pervDynEnv = DynamicEnv.bind(vCore,core,
			     DynamicEnv.bind(vInitial,initial,
			       DynamicEnv.bind(vMath,math,
				 DynamicEnv.empty)))
       in {static=pervStatEnv, dynamic=pervDynEnv, inverse=pervInvEnv}
      end

end (* structure BootEnv *)
