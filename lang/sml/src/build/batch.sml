(* Copyright 1989 by AT&T Bell Laboratories *)
functor Batch(structure M: CODEGENERATOR and A:ASSEMBLER)  : sig end =
struct

val pr = System.Print.say
open PrintUtil CompUtil ProcessFile System.Print
structure CGoptions = System.Control.CG

(* command parsing *)

fun skip_white stream =
    case lookahead stream
      of " " => (input(stream,1); skip_white stream)
       | "\t" => (input(stream,1); skip_white stream)
       | "\n" => (input(stream,1); skip_white stream)
       | _ => ()

fun getword stream =
    let val nextchar = input(stream,1)
     in case nextchar
	  of "" => ""
	   | " " => ""
	   | "\t" => ""
	   | "\n" => ""
	   | _ => nextchar ^ getword stream
    end

(* printDepth *)

val _  = signatures := 0

(* compilation static environment *)
val compenv = ref({static = StaticEnv.empty, inverse = InverseEnv.empty})

(* The commandline interpreter *)

val srcDir = ref ""
val dstDir = ref ""
val globalhandle = ref true
val dumpCore = ref false

fun compile env fname = 
    let val file = !srcDir ^ fname
        fun p(function,s) = 
	    let fun complain _ s = (pr(file ^ ": " ^ s ^ "\n"); raise Abort)
		val code = M.generate(function,NONE,complain)
		val codesize = String.size(System.Unsafe.cast code)
		val outfile = open_out(!dstDir ^ s ^ ".mo")
	     in System.Stats.codesize := codesize + !System.Stats.codesize;
		outputc outfile code;
		close_out outfile
	    end
     in pr("[Compiling " ^ file ^ "]\n");
	process(env, file, SOME p)
    end

fun assemble env s = 
    let val file = !srcDir ^ s
        fun p(function,s) = 
	    let fun complain _ s = (pr(file ^ ": "^s); raise Abort)
		val outfile = open_out(!dstDir ^ s ^ ".s")
	     in A.generate ((function,NONE,complain), outfile);
		close_out outfile
	    end
     in pr("[Assembling " ^ file ^ "]\n");
	process(env,file,SOME p)
    end

fun load env s = 
    let val file = !srcDir ^ s
     in pr ("[Loading " ^ file ^ "]\n");
	process(env,file,NONE)
    end

fun export s =
    let val file = !srcDir ^ s
	val msg = System.version ^ " (batch compiler)\n"
     in pr("[Exporting to " ^ file ^ "]\n"); exportML file; pr msg
    end

exception Notfound_Compile of string
local open System.Control 
      open CG
      val flags = [
		("tailrecur",tailrecur),
		("recordopt",recordopt),
		("tail",tail),
		("allocprof",allocprof),
		("closureprint",closureprint),
	        ("lambdaopt",lambdaopt),
		("cpsopt",cpsopt),
		("path",path),
		("betacontract",betacontract),
		("eta",eta),
		("selectopt",selectopt),
		("dropargs",dropargs),
		("deadvars",deadvars),
		("flattenargs",flattenargs),
		("switchopt",switchopt),
		("handlerfold",handlerfold),
		("branchfold",branchfold),
		("arithopt",arithopt),
		("betaexpand",betaexpand),
		("unroll",unroll),
		("unroll_recur",unroll_recur),
		("newconreps",newconreps),
		("knownfiddle",knownfiddle),
		("lambdaprop",lambdaprop),
		("invariant",invariant),
		("hoistup",hoistup),
		("hoistdown",hoistdown),
		("recordcopy",recordcopy),
		("tagopt",tagopt),
		("machdep",machdep),
		("misc1",misc1),
		("misc2",misc2),
		("hoist",hoist),
		("argrep",argrep),
		("reduce",reduce),
		("alphac",alphac),
		("comment",comment),
		("foldconst",foldconst),
		("etasplit",etasplit),
		("printit",printit),
		("printLambda",printLambda),
		("printsize",printsize),
		("scheduling",scheduling),
		("internals",internals),
		("MC.printArgs",MC.printArgs),
		("MC.printRet",MC.printRet),
		("MC.bindContainsVar",MC.bindContainsVar),
		("MC.bindExhaustive",MC.bindExhaustive),
		("MC.matchExhaustive",MC.matchExhaustive),
		("MC.matchRedundant",MC.matchRedundant),
		("MC.expandResult",MC.expandResult),
		("saveLvarNames",Access.saveLvarNames),
		("saveLambda",saveLambda),
                ("markabsyn",markabsyn),
		("debugging",debugging),
		("debugLook",debugLook),
		("debugBind",debugBind),
		("timings",timings),
		("dumpCore",dumpCore),
		("globalhandle",globalhandle),
		("indexing",System.Control.indexing),
		("ifidiom",ifidiom),
		("uncurry",uncurry),
		("cse",cse),
		("csehoist",csehoist),
		("rangeopt",rangeopt),
		("comparefold",comparefold),
		("extraflatten",extraflatten),
		("profiling",System.Unsafe.profiling),
		("floatreg_params",floatreg_params),
		("icount",icount),
                ("representations",representations)]
in
fun getflag f =
    let fun get nil = raise Notfound_Compile f
	  | get ((name,flag)::tl) = if f=name then flag else get tl
     in get flags
    end

fun printflags () =
    (pr "[Flags:\n";
     app (fn(name,flag:bool ref) => (pr name; pr " = "; pr(makestring(!flag)); pr "\n"))
	 flags;
     pr "]\n")
end

fun toggle "" = printflags()
  | toggle arg =
    let val flag = getflag arg
	val new = not(!flag)
    in pr ("["^arg^" := "^makestring new^"]\n"); flag := new
    end

fun lsave () = (toggle "saveLambda"; toggle "saveLvarNames")

fun atoi s =
    let val dtoi = fn "0" => 0 | "1" => 1 | "2" => 2 | "3" => 3 | "4" => 4
		    | "5" => 5 | "6" => 6 | "7" => 7 | "8" => 8 | "9" => 9
		    | _ => (pr "[garbled integer input]\n"; raise Abort)
    in case explode s
	of "~" :: s' => ~ (revfold (fn(a,b) => b * 10 + dtoi a) s' 0)
	 | s' => revfold (fn(a,b) => b * 10 + dtoi a) s' 0
    end

fun gcmessage() =
    let val f = System.Control.Runtime.gcmessages
    in f := (!f + 1) mod 4; pr "[gcmessages := "; pr(makestring(!f)); pr "]\n"
    end

fun summary() =
    (System.Stats.summary();
     pr "Generated code for:\n";
     pr(makestring(!System.Control.CG.knownGen));
     pr " known functions\n";
     pr(makestring(!System.Control.CG.knownClGen));
     pr " known functions with closures\n";
     pr(makestring(!System.Control.CG.escapeGen));
     pr " escaping functions\n";
     pr(makestring(!System.Control.CG.calleeGen));
     pr " callee-save continuations\n";
     pr(makestring(!System.Control.CG.spillGen));
     pr " spills\n";
     ())

val intvars = [("ratio",System.Control.Runtime.ratio),
	       ("maxregs",System.Control.CG.maxregs),
	       ("misc3",System.Control.CG.misc3),
	       ("misc4",System.Control.CG.misc4),
	       ("knownGen",System.Control.CG.knownGen),
	       ("knownClGen",System.Control.CG.knownClGen),
	       ("escapeGen",System.Control.CG.escapeGen),
	       ("calleeGen",System.Control.CG.calleeGen),
	       ("spillGen",System.Control.CG.spillGen),
	       ("softmax",System.Control.Runtime.softmax),
	       ("bodysize",System.Control.CG.bodysize),
	       ("rounds",System.Control.CG.rounds),
	       ("reducemore",System.Control.CG.reducemore),
	       ("targeting",System.Control.CG.targeting),
	       ("closureStrategy",System.Control.CG.closureStrategy),
	       ("calleesaves",System.Control.CG.calleesaves),
	       ("floatargs",System.Control.CG.floatargs),
	       ("floatvars",System.Control.CG.floatvars),
	       ("signatures",signatures)]

val otherexecs =        
       [("lsave",lsave),
	("summary",summary),
	("prFun",fn () =>
		prFun(atoi(skip_white std_in; getword std_in))),
	("gcmessages",gcmessage),
	("flushstdout",fn () => set_term_out(!System.Print.out,true)),
	("dumpMap",dumpMap),
	("asBoot",fn () =>
	   let val (env,_) = BootEnv.bootEnv assemble
	    in compenv := env
	   end),
	("mBoot",fn () =>
	   let val (senv,_) = BootEnv.bootEnv compile
	    in compenv := senv
	   end),
	("primeEnv",fn () => (compenv := {static=Prim.primEnv,
                                          inverse=InverseEnv.empty})),
	("allocReport",AllocProf.print_profile_info),
	("allocReset",AllocProf.reset) (* ,
	("clear",System.Control.Profile.clear),
	("reset",System.Control.Profile.reset),
	("report",fn () => System.Control.Profile.report (!System.Print.out)),
	("profileOff",System.Control.Profile.profileOff),
	("profileOn",System.Control.Profile.profileOn) *)]

val execs = otherexecs
       @ map (fn (s,r) => ("set"^s, fn()=>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "["; pr s; pr " := "; pr(makestring i); pr "]\n";
		    r := i
		end)) intvars


fun getexec f =
    let fun get nil = raise Notfound_Compile f
	  | get ((name,exec)::tl) = if f=name then exec else get tl
     in get execs
    end

fun printexecs () =
    (pr "[Available execs:\n";
     app (fn ("prFun",_) => pr "prFun <lvar>\n"
	   | ("printslots",_) => pr "printslots <structure>\n"
	   | (name,_) => (pr name; pr "\n"))
	 otherexecs;
     app (fn (s,r) => (pr "set"; pr s; pr " <int>  (currently ";
		       pr(makestring(!r)); pr ")\n"))
         intvars;
     pr "]\n")

fun execute "" = printexecs()
  | execute arg =
    let val exec = getexec arg
    in  pr("["^arg^"()]\n");
	exec()
    end

fun help() = pr "\
\!file      => compile the file.\n\
\*file      => assemble the file.\n\
\<file      => parse the file.\n\
\>file      => export to a file.\n\
\%          => print the last generated lambda.\n\
\#word      => comment; ignored.\n\
\@directory => look for files in a directory (directory should end in /).\n\
\&directory => put mo files in a directory (directory should end in /).\n\
\~function  => execute a function.\n\
\^flag      => toggle a flag.\n\
\?          => print this help message.\n"

fun interp "" = ()
  | interp word =
    let val arg = substring(word,1,size word - 1) handle Substring => ""
    in  (case substring(word,0,1) of
 	      "!" => compenv := compile (!compenv) arg
	    | "*" => compenv := assemble (!compenv) arg
	    | "<" => compenv := load (!compenv) arg
	    | ">" => export arg
	    | "%" => ProcessFile.prLambda()
	    | "#" => ()			(* comment *)
	    | "@" => srcDir := arg	(* change load directory *)
	    | "&" => dstDir := arg	(* change load directory *)
	    | "~" => execute arg	(* execute function *)
	    | "^" => toggle arg		(* toggle flag *)
	    | "?" => help()		
	    |  _  => pr ("[What is \""^word^"\"?]\n")
	) handle e as Notfound_Compile f =>
		   (pr("[flag \""^f^"\" not recognized]\n");
		    raise e)
    end

exception INTERRUPT
fun setcont () = (
      System.Unsafe.toplevelcont := callcc(fn k => (
      callcc(fn k' => (throw k k'));
      raise INTERRUPT)))
fun interp1 word = if !globalhandle
      then (setcont(); interp word)
      handle Abort => (
              pr "[Failed on "; pr_mlstr word; pr "]\n"; System.Print.flush())
           | INTERRUPT => (pr "\n[Interrupt]\n"; System.Print.flush())
           | e => (
              pr "[Failed on "; pr_mlstr word; pr " with ";
              pr(System.exn_name e); pr "]\n"; System.Print.flush())
      else (setcont(); interp word)
      handle e => (
          case e
           of INTERRUPT => (pr "\n[Interrupt]\n")
            | _ => (pr "[Failed on "; pr_mlstr word; pr " with ";
                    pr(System.exn_name e); pr "]\n");
	    System.Print.flush();
	    if !dumpCore
	    then (toggle "globalhandle";
		  toggle "dumpCore";
                  pr "[Saving state]\n[Exporting to sml.save]\n";
		  System.Print.flush();
                  if exportML "sml.save"
                    then pr "hello there\n"
                    else (summary(); raise e))
            else raise e)
		 
(* command-line interpreter top-level loop *)
fun toplevel () =
    if end_of_stream std_in
    then ()
    else (skip_white std_in;
	  if (end_of_stream std_in)
	  then () 
	  else (interp1(getword std_in); 
		compenv := Environment.consolidateStatic(!compenv);
		toplevel ()))


(* load the pervasives into top-level env (no .mo files generated) *)
val _ = 
  let val (env,_) = BootEnv.bootEnv load      
   in compenv := Environment.consolidateStatic(env)
  end

(* start up command interpreter *)
val _ = (pr "Batch Interpreter\n";
	 toplevel ())

end
