(* Copyright 1989,1992 by AT&T Bell Laboratories *)

signature EVALLOOP =
sig
  type interactParams
  val interact : interactParams -> unit
  val eval_stream : interactParams -> string * instream -> unit
end

structure EvalLoop :  EVALLOOP =
struct
  open ErrorMsg Environment Elaborate CompUtil System.Timer

  type interactParams =
      {baseEnv:Environment.environment, 
       localEnvRef:Environment.environment ref,
       parser: Source.inputSource -> Modules.env ->  Elaborate.parseResult,
       generate: CPS.function * System.Unsafe.object option * 
                  ErrorMsg.complainer -> string,
       perform: (unit -> System.Unsafe.object vector) -> 
	        System.Unsafe.object vector,
       isolate : (((unit -> System.Unsafe.object vector) -> 
		       System.Unsafe.object vector) ->
		  System.Unsafe.object vector) ->
		 ((unit -> System.Unsafe.object vector) -> 
		     System.Unsafe.object vector) ->
		 System.Unsafe.object vector,
       printer: Environment.statenv -> PrettyPrint.ppstream -> Absyn.dec -> 
                (Access.lvar -> System.Unsafe.object) -> unit}

  structure U = System.Unsafe
  val update = System.Stats.update

  (* toplevel loop *)

  (* Convert body to fn perform => perform(fn () => body). 
     Allows debugger to obtain control after lvars have been bound
     to dynamic environment but before execution of body. *)
  fun performable lambda = 
      let open Lambda Access BasicTypes
	  val perform = namedLvar(Symbol.varSymbol "perform")
          val pty = ARROWty(ARROWty(INTty,BOXEDty),BOXEDty)
	  val unitLvar = mkLvar()

      in FN(perform, pty, APP(VAR perform,FN(unitLvar,INTty,lambda)))
      end

  fun codegen generate
              (lambda,
	       source as {anyErrors,lineNum,...}:Source.inputSource) =
      let val complainer = ErrorMsg.error source (!lineNum,!lineNum)
	  val lambda' = convert(lambda)
	  val _ = if !anyErrors then raise Abort else ()
	  val code = System.Code.mkCode(generate(lambda',NONE,complainer))
	  val _ = if !anyErrors then raise Abort else ()
	  val codesize = System.Code.sizeOf code
	  val executable :
		  (int -> U.object)
		    -> ((unit -> U.object vector) -> U.object vector)
		    -> U.object vector
		= (debugmsg("about to boot\ncode size ="
			    ^ Integer.makestring(codesize));
		   System.Code.apply code)
       in System.Stats.codesize := codesize + !System.Stats.codesize;
	  executable
      end

  fun evalLoop ({baseEnv, localEnvRef, parser, generate,
		 perform, isolate, printer} : interactParams)
               (source: Source.inputSource) : unit =
    let val parse = parser source
	fun loop () =
      (* perform one transaction per loop *)
      let val baseStatEnv = #static baseEnv
          val baseInvEnv = #inverse baseEnv
          val baseDynEnv = #dynamic baseEnv
          
	  val localEnv = !localEnvRef
          val statenv = StaticEnv.atop(#static localEnv,baseStatEnv)
          val invenv = InverseEnv.atop(#inverse localEnv,baseInvEnv)
	  val dynenv = DynamicEnv.atop(#dynamic localEnv,baseDynEnv)

          val getty = gengetty(invenv)

       in U.toplevelcont := 
		 callcc(fn k => (callcc(fn k' => (throw k k'));
				 raise Interrupt));
	  case parse statenv
	    of ABORT => raise Abort
	     | ERROR => raise Abort
	     | EOF => raise Eof
	     | PARSE (absyn,deltaStatEnv) => (* normal program *)
		let val statenv' = StaticEnv.atop(deltaStatEnv,statenv)

		    val (newlvars,lambda) = translate(statenv',absyn,source)
		    val _ = if !(#anyErrors source) then raise Abort else ()

		    val lambda = Opt.closetop(performable lambda,
					      !CoreInfo.corePath,getty)

		    val executable = if !System.Control.interp
				     then Interp.interp lambda
				     else codegen generate (lambda,source)

		    val looker = DynamicEnv.look dynenv

		    val result = 
			let val _ = U.Assembly.current := 0
			    val timer = start_timer()
			    val result = isolate (executable looker) perform
			    val time = check_timer timer
			 in U.Assembly.current := 1;
			    update(System.Stats.execution,time);
			    infomsg "execution" time;
			    result
			end

                    val {static=senv_delta,inverse=ienv_delta} =
                                            makeStaticEnv(deltaStatEnv)
		    val newLocalEnv = bindEnv(senv_delta,ienv_delta,
					      newlvars,result,!localEnvRef)
			(* refetch localEnvRef because execution may
			   have changed its contents *)

		    val newLocalEnv = consolidateEnv newLocalEnv
			(* cleanup static environment. May be overkill
			   to do this on each transaction. *)

		    val newStatEnv = 
			StaticEnv.atop(#static newLocalEnv, baseStatEnv)

		 in PrettyPrint.with_pp (#errConsumer source)
		     (fn ppstrm =>
		        printer newStatEnv ppstrm absyn
		         (DynamicEnv.look(#dynamic newLocalEnv)));
                    Index.report source (absyn, newStatEnv);
		    localEnvRef := newLocalEnv;
		    loop()
		end
      end
    in loop()
    end

  (* interactive loop, with error handling *)
  fun interact interactParams : unit =
      let val source = Source.newSource("std_in",1,std_in,true,
					ErrorMsg.defaultConsumer(),
					NONE);
	  fun flush() = input(std_in,(can_input std_in)) handle Io _ => ""
	  fun loop () =
	      evalLoop interactParams source
	      handle Eof => ()
		   | Interrupt => (System.Print.say "\nInterrupt\n"; 
				   flush(); loop())
		   | Abort => (flush(); loop())
		   | Error => (flush(); loop())
                   | Match => (
                        System.Print.say "\nuncaught Match exception ";
                        System.Print.say (!System.errorMatch);
                        System.Print.say "\n"; flush(); loop ())
                   | Bind => (
                        System.Print.say "\nuncaught Bind exception ";
                        System.Print.say (!System.errorMatch);
                        System.Print.say "\n"; flush(); loop ())
		   | Io s =>
		       (System.Print.say("\nuncaught exception Io \"" ^ s ^ "\"\n");
			flush(); loop())
		   | Fail s =>
		       (System.Print.say("\nuncaught exception Fail \"" ^ s ^ "\"\n");
			flush(); loop())
		   | exn =>
		       (System.Print.say("\nuncaught exception "
			       ^ System.exn_name exn ^ "\n");
			flush(); loop())
       in loop()
      end (* interact *)

  fun eval_stream interactParams (fname:string,stream:instream) : unit =
      let val interactive = is_term_in stream
	  val source = 
	      Source.newSource(fname,1,stream,interactive,
			       ErrorMsg.defaultConsumer(),
			       if not interactive 
			       then Index.openIndexFile fname
			       else NONE)
       in evalLoop interactParams source
	  handle exn =>
	    (Source.closeSource source;
	     case exn
	      of Eof => ()
	       | _ => raise exn)
      end (* eval_stream *)

end (* struct EvalLoop *)

