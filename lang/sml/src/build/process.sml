(* Copyright 1989 by AT&T Bell Laboratories *)

signature PROCESSFILE =
sig
  val dumpMap : unit -> unit
  val prLambda : unit -> unit
  val prFun : int -> unit
  val process : Environment.staticEnv * string * (CPS.function * string 
                    -> unit) option -> Environment.staticEnv
end

structure ProcessFile : PROCESSFILE =
struct
  open Access Modules Types Variables PrintUtil ModuleUtil
       ErrorMsg Absyn Lambda CompUtil

  fun for l f = app f l

  val saveLambda = System.Control.saveLambda
  val lambda = ref (Lambda.RECORD [])
  (* really needed only for interactive version *)
  val _ = System.Control.prLambda :=
          (fn () => (MCprint.printLexp(!lambda); newline()))
  fun prLambda() = (MCprint.printLexp(!lambda); newline())
  fun prFun lv = (MCprint.printFun(!lambda) lv; newline())


  (* lvar -> string environment used by batch compiler to map module
     lvars to names of modules *)
  exception Modname
  val m : string Intmap.intmap = Intmap.new(32, Modname)
  val lookup = Intmap.map m
  val enterName = Intmap.add m
  fun lookupName v =
      lookup v 
      handle Modname => 
	 ErrorMsg.impossible ("Bad free variable: " ^ Access.lvarName v)
  fun dumpMap() =
      let val say = System.Print.say
	  fun p(i:int,s:string) = app say [makestring  i, " -> ", s, "\n"]
      in  say "lvar -> structure mapping:\n"; Intmap.app p m
      end

  fun process({static=env,inverse}, fname, gencode) : Environment.staticEnv =
      let val stream = open_in fname
	  val source as {anyErrors,...} =
		Source.newSource(fname,1,stream,false,
				 ErrorMsg.defaultConsumer(),
				 Index.openIndexFile fname)
	  val cummEnv = ref env
          val cummInv = ref inverse

	  fun proc(name,lvar,mkLam) =
	    let val _ = enterName(lvar, name)
                val getty = gengetty(!cummInv) 
                val lam = Opt.closestr(lookupName,getty,mkLam(), 
                                       !CoreInfo.corePath)
	     in debugmsg "closed";
                if !saveLambda then lambda := lam else ();
                (case gencode
       	          of NONE => ()
	           | SOME gencode => gencode(convert lam, name));
	        if !anyErrors then raise Abort else ()
	    end

	  fun comp absyn =
	      let fun pr () = 
		      PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
		       (fn ppstrm =>
			 PPDec.ppDec (!cummEnv) ppstrm absyn
			   (fn _ => impossible "Process.f"))
	       in case absyn
		    of (SEQdec decs) => app comp decs
		     | (MARKdec(d,_,_)) => comp d
		     | (SIGdec sl) => pr ()
		     | (OPENdec _) => pr ()
		     | (FSIGdec _) => pr ()
		     | (STRdec sbs) =>
			 (pr ();
			  for sbs
			    (fn sb as
				STRB{strvar as STRvar{name=n,
						      access=PATH[v],...},
				     ...} =>
			       let fun mkLam() = transStrb(!cummEnv,sb,source)
				in proc(Symbol.name n, v, mkLam)
			       end))
		     | (ABSdec sbs) =>
			 (pr ();
			  for sbs
			    (fn sb as
				STRB{strvar as STRvar{name=n,
						      access=PATH[v],...},
				     ...} =>
			       let fun mkLam() = transStrb(!cummEnv,sb,source)
				in proc(Symbol.name n, v, mkLam)
			       end))
		     | (FCTdec fbs) =>
			 (pr ();
			  for fbs
			    (fn fb as
				FCTB{fctvar as FCTvar{name,
						      access=PATH[v],...},
				     ...} =>
			       let fun mkLam () =
				       transFctb(!cummEnv,fb,source)
				       handle Match => impossible
					  "transFctb: match exception"
				in proc(Symbol.name name, v, mkLam)
			       end))
		     | _ => error source (0,0) COMPLAIN
			     "signature, functor, or structure expected"
			     nullErrorBody
	      end

	  val parser = Elaborate.parse (fn dec => dec) source

	  fun loop () =
	      case (parser (!cummEnv))
		of Elaborate.EOF =>
		     (Source.closeSource source;
		      if !anyErrors then raise Abort 
                      else {static= !cummEnv,inverse= !cummInv})
		 | Elaborate.ABORT =>
		     (Source.closeSource source; raise Abort)
		 | Elaborate.ERROR =>
		     (Source.closeSource source; raise Abort)
		 | Elaborate.PARSE(absyn,envr) =>
		    let val {static=senv,inverse=ienv} = 
                            Environment.makeStaticEnv(envr)
                        val _ = (cummEnv := Env.atop(senv,!cummEnv))
                        val _ = (cummInv := InverseEnv.atop(ienv,!cummInv))
                     in comp absyn;
		        loop()
                    end

       in loop ()
      end (* fun process *)

end (* structure ProcessFile *)
