(* Copyright 1989,1992 by AT&T Bell Laboratories *)

funsig DEBUGGER(structure Machm : CODEGENERATOR) = sig end

functor BogusDebugger(structure Machm : CODEGENERATOR) : sig end = struct end

functor RealDebugger(structure Machm : CODEGENERATOR) : sig end =
struct
  open ErrorMsg CompUtil Elaborate Environment System.Timer PrettyPrint
  structure U = System.Unsafe

  val update = System.Stats.update
  val say = System.Print.say

  (* shared with dbguser/interface.sml *)
  datatype debuglevel = 
       FULL
     | LIVE of ((string * instream) option * (unit -> unit) * (unit -> unit))
     | INTERPOLATION

  val _ = DebugUtil.debugStatEnv := #static (!pervasiveEnvRef)

  fun instrument (source: Source.inputSource,
	          statEnv: StaticEnv.statenv,
	          absyn: Absyn.dec) : Absyn.dec =
	let fun dump label absyn =
		if !System.Control.debugging then
		  (with_pp (ErrorMsg.defaultConsumer())
		     (fn ppstrm =>
		       (add_string ppstrm ("\n" ^ label ^ "\n");
			PPAbsyn.ppDec (statEnv,(SOME source)) ppstrm (absyn,1000);
			add_newline ppstrm)))
		else ()
            val _ = dump "BEFORE:" absyn
            val timer = start_timer ()
	    val firstPlace = DebugStatic.nextPlace()
	    val lastBindTime  = 
	       if DebugExec.inCompUnit() then  (* interpolation *)
		 DebugExec.currentTime()
	       else DebugStatic.lastUnitTime()
	    val {absyn,events,evns} =
	       DebugInstrum.instrumDec{absyn=absyn,
				       firstPlace=firstPlace,
				       lastBindTime=lastBindTime}
            val time = check_timer timer
        in update(System.Stats.debuginstrum,time);
	   infomsg "debug instrument" time;
           dump "AFTER:" absyn;
           DebugStatic.install{inputSource=source,
			       firstPlace=firstPlace,
			       events=events,
			       evns=evns};
	   absyn
        end
		
  (* Transformation function guarantees that every function argument
     and every function rule pattern has a "simple" type, i.e., has
     an obvious base type or an explicit type constraint. 
     This notion of simplicity must match that used in DebugBindings. *)
  val constrainer : Absyn.dec -> Absyn.dec =
      let open Absyn Variables TypesUtil
	  fun constrainDec dec =
	     (case dec of
		VALdec vbl => VALdec(map constrainVb vbl)
	      | VALRECdec rvbl => VALRECdec(map constrainRvb rvbl)
	      | ABSTYPEdec{abstycs,withtycs,body} =>
		    ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,
			       body=constrainDec body}
	      | STRdec strbl => STRdec(map constrainStrb strbl)
	      | ABSdec strbl => ABSdec(map constrainStrb strbl)
	      | FCTdec fctbl => FCTdec(map constrainFctb fctbl)
	      | LOCALdec(decin,decout) =>
		    LOCALdec(constrainDec decin,constrainDec decout)
	      | SEQdec decl => SEQdec(map constrainDec decl)
	      | MARKdec(dec,l1,l2) => MARKdec(constrainDec dec,l1,l2)
	      | dec => dec)
          and constrainExp exp =
	     (case exp of
		RECORDexp lexpl =>
		    RECORDexp(map (fn (l,exp) => (l,constrainExp exp)) lexpl)
	      | SEQexp expl => SEQexp (map constrainExp expl)
	      | APPexp (exp1,exp2) =>
		  let fun simple exp =
		       (case exp of
			  INTexp _ => exp
			| REALexp _ => exp
			| STRINGexp _ => exp
			| CONSTRAINTexp(exp,ty) => 
			      CONSTRAINTexp(constrainExp exp,ty)
			| MARKexp(exp,l1,l2) => MARKexp(simple exp,l1,l2)
			| _ => CONSTRAINTexp(constrainExp exp,mkMETAty()))
		  in APPexp(constrainExp exp1,simple exp2)
		  end
	      | CONSTRAINTexp (exp,ty) => CONSTRAINTexp(constrainExp exp,ty)
	      | HANDLEexp(exp1,HANDLER exp2) =>
		    HANDLEexp(constrainExp exp1,HANDLER (constrainExp exp2))
	      | RAISEexp(exp,t) => RAISEexp(constrainExp exp,t)
	      | LETexp(dec,exp) => LETexp(constrainDec dec,constrainExp exp)
	      | CASEexp(exp,rl) => CASEexp(constrainExp exp,
					   map constrainRule rl)
	      | FNexp(rl,t) => FNexp(map constrainRule rl,t)
	      | MARKexp(exp,l1,l2) => MARKexp(constrainExp exp,l1,l2)
	      | _ => exp)
	  and constrainRule(RULE(pat,exp)) =
  	        RULE(constrainPat pat,constrainExp exp)
	  and constrainPat pat =
	       (case pat of 
		  WILDpat => pat
		| VARpat(VALvar _) => pat
		| INTpat _ => pat
		| REALpat _ => pat
		| STRINGpat _ => pat
	        | CONSTRAINTpat _ => pat
		| _ => CONSTRAINTpat(pat,mkMETAty()))
          and constrainVb (VB{pat,exp,tyvars}) =
	        VB{pat=pat,exp=constrainExp exp,tyvars=tyvars}
	  and constrainRvb (RVB{var,exp,resultty,tyvars}) =
	        RVB{var=var,exp=constrainExp exp,
		    resultty=resultty,tyvars=tyvars}
	  and constrainStrb (STRB{strvar,def,thin,constraint}) =
	        STRB{strvar=strvar,def=constrainStrexp def,thin=thin,
		     constraint=constraint}
	  and constrainStrexp strexp =
	       (case strexp of
		  VARstr sv => VARstr sv
		| STRUCTstr{body,str,locations} =>
		      STRUCTstr{body=map constrainDec body,str=str,
				locations=locations}
		| APPstr{oper,argexp,argthin,str} =>
		      APPstr{oper=oper,argexp=constrainStrexp argexp,
			     argthin=argthin,str=str}
		| LETstr(dec,strexp) =>
		      LETstr(constrainDec dec,constrainStrexp strexp)
		| MARKstr(strexp,l1,l2) =>
		      MARKstr(constrainStrexp strexp,l1,l2))
	  and constrainFctb (FCTB{fctvar,def}) =
	        FCTB{fctvar=fctvar,def=constrainFctexp def}
	  and constrainFctexp fctexp =
	     (case fctexp of
		VARfct vfct => VARfct vfct
	      | LETfct (dec,fct) => 
		    LETfct(constrainDec dec, constrainFctexp fct)
	      | FCTfct{param,def,thin,constraint} =>
		    FCTfct{param=param,def=constrainStrexp def,
			   thin=thin,constraint=constraint})
      in constrainDec
      end

  (* Filter output of successful parses through instrumenter. *)
  fun parseAndInstrument (source: Source.inputSource) =
      let val parse' = parse constrainer source 
          fun parseit (statEnv:statenv) : parseResult =
	      case parse' statEnv 
		of PARSE(absyn,deltaStatEnv) => 
	 	     PARSE(instrument(source,
				      StaticEnv.atop(deltaStatEnv,statEnv),
				      absyn),
			   deltaStatEnv)
	      | x => x
      in parseit
      end
 
  (* Filter printing of top-level declarations from instrumented code. 
     Debugger declarations begin with an underscore (_). *)
  fun debugPrintDec statenv ppstrm absyn looker =
      let open Absyn Variables
	  fun cleanVb (VB{pat,exp,tyvars}) =
	        let fun cleanPat(pat as VARpat(VALvar{name=[n],...})) =
			  if substring(Symbol.name n,0,1) = "_" then
			    WILDpat
			  else pat
		      | cleanPat(RECORDpat{pats=ref pl,fields,flex,typ}) =
			  RECORDpat{pats=ref (map cleanPat pl),
				    fields=map (fn (l,p) => (l,cleanPat p)) 
				               fields,
				    flex=flex,typ=typ} (* ?? *)
		      | cleanPat(APPpat(con,t,pat)) =
			  APPpat(con,t,cleanPat pat)
		      | cleanPat(CONSTRAINTpat(pat,ty)) =
			  CONSTRAINTpat(cleanPat pat,ty)
		      | cleanPat(LAYEREDpat(pat1,pat2)) =
			  LAYEREDpat(cleanPat pat1, cleanPat pat2)
		      | cleanPat pat = pat
		in VB{pat=cleanPat pat,exp=exp,tyvars=tyvars}	  
		end
          fun cleanDec (VALdec vbs) = VALdec (map cleanVb vbs)
	    | cleanDec (ABSTYPEdec{abstycs,withtycs,body}) =
               ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=cleanDec body}
	    | cleanDec (LOCALdec(decIn,decOut)) =
	                    LOCALdec(decIn, cleanDec decOut)
	    | cleanDec (SEQdec decs) = SEQdec(map cleanDec decs)
	    | cleanDec (MARKdec(dec,l1,l2)) = MARKdec(cleanDec dec,l1,l2)
	    | cleanDec dec = dec
      in PPDec.ppDec statenv ppstrm (cleanDec absyn) looker
     end


  val debugEnv = DebugEnv.debugEnvironment

  val debuggerCommandsEnvRef = ref emptyEnv  (* to speed up lookups *)
  val debugMonitorEnvRef = ref (!debuggerCommandsEnvRef)

  fun init FULL f =
      let open DebugMotions DebugStatic DebugUtil
      in case runCompUnit complete f of
           NORMAL r =>  
	       (say "[debugging support included]\n";
		r)
	 | EXCEPTION e => 
	       (rollback(); raise e)
	 | ABORT => (* shouldn't happen *)
	       (rollback(); raise Abort)
	 | INTERRUPT =>
	       (rollback(); raise Interrupt)
      end
   | init (LIVE(control,startUp,abortShutDown)) f =
      let open DebugMotions DebugStatic System.Control DebugUtil
	  val oldPrimaryPrompt = !primaryPrompt
	  and oldSecondaryPrompt = !secondaryPrompt
	  fun debugMonitor() = 
	      (debugMonitorEnvRef := !debuggerCommandsEnvRef;
	       say "[ready to execute under debugger]\n";
	       startUp();
	       let val baseEnv0 = layerEnv(!topLevelEnvRef,
					   !pervasiveEnvRef)
		   val baseEnv = layerEnv(debugEnv,baseEnv0)
		   val innerLoopParams = 
		       {baseEnv=baseEnv,
			localEnvRef=debugMonitorEnvRef,
			parser=Elaborate.parse (fn dec => dec),
			generate=Machm.generate,
			perform=(fn exec => exec()),
			isolate=(fn f => fn x => f x),
			printer=PPDec.ppDec}
	       in primaryPrompt := "[dbg]" ^ oldPrimaryPrompt;
		  secondaryPrompt := "[dbg]" ^ oldSecondaryPrompt;
		  debugStatEnv := #static baseEnv0;
		  case control of
		    SOME (fname,stream) => 
		       EvalLoop.eval_stream innerLoopParams (fname,stream)
		  | NONE => EvalLoop.interact innerLoopParams;
		  (* return only via ctrl/d or stream error *)
		  abortShutDown();
		  abort();
		  debugPanic "Returned from abort"
	       end)
	  fun reset() = 
	     (topLevelEnvRef := 
	          layerEnv(!debugMonitorEnvRef,!topLevelEnvRef);
	      primaryPrompt := oldPrimaryPrompt;
	      secondaryPrompt := oldSecondaryPrompt)
      in case runCompUnit debugMonitor f of 
	   NORMAL r =>
	       (say "[completing normal execution]\n";
		reset();
		r) 
	 | EXCEPTION e => 
	       (say "[execution terminated by exception]\n";
		rollback();
		reset();
		raise e)
	 | ABORT => 
	       (say "[execution aborted]\n";
		rollback();
		reset();
		raise Abort)
	 | INTERRUPT =>
	       (say "[execution interrupted]\n";
		rollback();
		reset();
		raise Interrupt)
     end
    | init INTERPOLATION f =
         (DebugMotions.interpolateCompUnit (fn () => (f(); ()));
	  raise Abort)
	  
  val debuggerPervasiveEnvRef = ref emptyEnv

  fun dbgGenerate x =
      let open System.Control.CG
	  val oldInvariant = !invariant
	  and oldUnroll = !unroll
	  and oldKnownfiddle = !knownfiddle
      in invariant := false;
	 unroll := false;
	 knownfiddle := false;
	 Machm.generate x
	 before
	 (invariant := oldInvariant;
	  unroll := oldUnroll;
	  knownfiddle := oldKnownfiddle)
      end

  fun dbgParams level =
      {baseEnv= !debuggerPervasiveEnvRef,
       localEnvRef=topLevelEnvRef,
       parser=parseAndInstrument,
       generate=dbgGenerate,
       perform=init level,
       isolate=CompUtil.isolate,
       printer=debugPrintDec} 

  fun use_file_dbg (level:debuglevel,fname:string) : unit = 
      (DebugStatic.hideFile fname;
       EvalLoop.eval_stream (dbgParams level)
              (fname,(open_in fname
		        handle Io s =>
			    (say(implode["[use failed: ",s,"]\n"]);
			     raise Error))))


  fun use_stream_dbg (level:debuglevel,stream:instream) : unit = 
      EvalLoop.eval_stream (dbgParams level) ("<instream>",stream)
      
  fun interpolate_stream(stream:instream) : unit =
      let val dummyEnvRef = ref emptyEnv
          val baseEnv = layerEnv(!debugMonitorEnvRef,
				 layerEnv(debugEnv,
					  layerEnv(!topLevelEnvRef,
						   !debuggerPervasiveEnvRef)))
      in EvalLoop.eval_stream 
	     {baseEnv = baseEnv,
	      localEnvRef = dummyEnvRef,
	      parser = parseAndInstrument,
	      generate = dbgGenerate,
	      perform = init INTERPOLATION,
	      isolate = (fn f => fn x => f x),
	      printer = (fn env => fn ppstrm => fn absyn => fn looker => ())}
	    ("<interpolation>",stream)
	    handle Abort => ()
      end
	 
  (* Set up ref which contains values needed by instrumented code at run time.
     This is not very neatly modularized! *)
  val _ = System.Control.Debug.getDebugf := 
	   (fn firstEvn =>
		U.cast
		(DebugKernel.times,
		 DebugStatic.evnTimesArray firstEvn,
		 DebugKernel.break,
		 DebugStore.hcreater,
		 U.Weak.weak,
		 DebugStore.updatedRList,
		 DebugStore.PCONS,
		 DebugStore.updatedAList,
		 Array.array))

  (* Set up values for user-space interface.  
     It is very important that types match those on user-space end,
     since the interface is not type-checked. *)
  val _ =
    let val old_interface = !System.Control.Debug.interface
    in System.Control.Debug.interface := 
      (fn 
         0 => U.cast debuggerPervasiveEnvRef
       | 1 => U.cast use_file_dbg
       | 2 => U.cast use_stream_dbg
       | 3 => U.cast DebugMotions.withEstablishedTime
       | 4 => U.cast DebugExec.currentTime
       | 5 => U.cast (fn() => DebugStatic.immediatePlaces(
			      DebugStatic.placesFor(DebugExec.currentEvn())))
       | 6 => U.cast (fn () => (!DebugExec.initialTime,!DebugExec.finalTime))
       | 7 => U.cast DebugQueries.lastTimes
       | 8 => U.cast DebugMotions.jump
       | 9 => U.cast DebugMotions.binSearch
       | 10 => U.cast DebugQueries.callTrace
       | 11 => U.cast DebugQueries.getVal
       | 12 => U.cast DebugQueries.printVal
       | 13 => U.cast DebugUtil.isFn
       | 14 => U.cast DebugQueries.printBind 
       | 15 => U.cast DebugUtil.debugdebug
       | 16 => U.cast (DebugStore.updatedAList,
		       DebugStore.updatedRList,
		       DebugStore.createdList,
		       DebugStore.hcreatea,
		       DebugStore.hcreater)
       | 17 => U.cast DebugStatic.eventPlacesAfter
       | 18 => U.cast DebugStatic.eventPlacesBefore
       | 19 => U.cast (DebugMotions.setHandler, (* special exn handling! *)
		       DebugSignals.inqHandler,
		       DebugSignals.maskSignals,
		       DebugSignals.pause)
       | 20 => U.cast DebugMotions.complete
       | 21 => U.cast DebugMotions.abort
       | 22 => U.cast DebugExec.inCompUnit
       | 23 => U.cast (fn () => !DebugExec.blockingExn)
       | 24 => U.cast DebugIO.logit
       | 25 => U.cast DebugQueries.eventDesc
       | 26 => U.cast DebugRun.maxTimeDelta
       | 27 => U.cast DebugKernel.times
       | 28 => U.cast DebugQueries.caller
       | 29 => U.cast DebugUtil.infinity
       | 30 => U.cast DebugEnv.setEnvTime
       | 31 => U.cast DebugQueries.atCall
       | 32 => U.cast DebugEnv.useSpecial
       | 33 => U.cast DebugStatic.charnoForLinepos
       | 34 => U.cast DebugUtil.sizereport
       | 35 => U.cast DebugInstrum.instrumLevel
       | 36 => U.cast DebugRun.memoLevel
       | 37 => U.cast DebugRun.dumpCache
       | 38 => U.cast DebugRun.dfactor
       | 39 => U.cast (fn () => DebugKernel.execTime)
       | 40 => U.cast DebugRun.maxStates
       | 41 => U.cast DebugRun.preCachingEnabled
       | 42 => U.cast DebugMotions.cpCost
       | 43 => U.cast DebugRun.pcfactor
       | 44 => U.cast DebugRun.zapFactor
       | 45 => U.cast DebugRun.strictLru
       | 46 => U.cast DebugRun.cacheRatio
       | 47 => U.cast DebugRun.zapCount
       | 48 => U.cast interpolate_stream
       | 49 => U.cast debuggerCommandsEnvRef
       | 50 => U.cast DebugMotions.setSignal
       | 51 => U.cast DebugMotions.clearSignal
       | 52 => U.cast (fn () => 
		       case DebugSignals.deliverableSignal() of
			 SOME(signal,_) => SOME signal
		       | NONE => NONE)
       | 53 => U.cast DebugSignals.setHalting
       | 54 => U.cast DebugQueries.exnArg
       | 55 => U.cast DebugStatic.lineposForCharno
       | q => old_interface q)
    end (* let ... *)
end

