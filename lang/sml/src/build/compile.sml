(* Copyright 1989,1991,1992 by AT&T Bell Laboratories *)

(* Modules compiler for New Jersey ML.  Nick Rothwell, LFCS, January 1989,
 *
 * revised and simplified by Andrew Appel, 1990. 
 *
 * Modified to support separate compilation with free
 * structure references. Also, code supporting import
 * command has been isolated for easy removal. (erg)
 *
 * "import" code has been removed, old ModuleComp code has been
 * incorporated, and name of functor has been changed to CompileUnit
 * from Importer.  Changed to use first-class environments and
 * to provide a minimal compilation interface to support separate
 * compilation.
 *)

signature COMPILEUNIT =
sig
  type staticUnit
  type codeUnit
  type compUnit
  val changeLvars: staticUnit -> staticUnit
  val elaborateUnit: Source.inputSource * Environment.staticEnv -> staticUnit
  val parseUnit: Source.inputSource * Environment.staticEnv 
		 -> Ast.dec * Environment.staticEnv
  val compileUnit: Source.inputSource * Environment.staticEnv -> compUnit
  val compileAst: Ast.dec * Environment.staticEnv * Source.inputSource option
                  -> compUnit
  val executeUnit: 
	compUnit * Environment.environment -> Environment.environment
end

functor CompileUnit(structure Machm : CODEGENERATOR) : COMPILEUNIT =
struct
  open PrintUtil Access Modules Environment Lambda

  type componentId = InverseEnv.componentId

  type staticUnit = {staticEnv: staticEnv,
		     boundLvars: lvar list}

  type codeUnit =   {executable: System.Code.code,
		     imports: componentId list}

  type compUnit =   staticUnit * codeUnit

  type looker = lvar -> System.Unsafe.object
  type result = System.Unsafe.object vector

  (* A single exception for all compilation failures. *)
  exception Compile = System.Compile.Compile

  fun fail msg = raise (Compile msg)

 (* changeLvars:
  * Rename the lvars of the static module.
  * For each module binding, a fresh lvar will be chosen; hence
  * at run-time, several imports of the same structure will presumably
  * lead to a new copy of the code of that structure.
  * Collect new import info.
  *)
  fun changeLvars ({staticEnv={static,...}, boundLvars}: staticUnit) : staticUnit =
      let val newlvars = map (fn _ => mkLvar()) boundLvars
	       (* New lvars to be bound to module values. *)

	  (* use intmap for translating lvars if there are many of them. *)
	  val lvarcount = length boundLvars
	  val lookup =
	      if lvarcount < 8
	      then (fn x =>
		      let fun f(a::ar, b::br) = if a=x then b else f(ar,br)
			    | f _ = ErrorMsg.impossible "CompileUnit 1"
		       in f(boundLvars,newlvars)
		      end)
	      else
	      let val table = Intmap.new(lvarcount, Compile("changeLvars"))
		  val add = Intmap.add table
		  fun fill_table([],_) = ()
		    | fill_table(old::l,new::m) = (add(old,new); fill_table(l,m))
	       in fill_table(boundLvars,newlvars);
		  Intmap.map table
	      end

	  val newstatenv = ref(StaticEnv.empty)
	  val newinvenv = ref(InverseEnv.empty)

	  fun adjustBinding(_,FCTbind(FCTvar{name,access=PATH[lvar], binding})) = 
		let val newlvar = lookup lvar
		    val newbind = FCTbind(FCTvar{name = name, access= PATH[newlvar],
						 binding = binding})
		 in newinvenv :=
		     InverseEnv.bind(newlvar,
				     {name=name,
				      pid=ModuleUtil.getFctStamp binding,
				      ty=TransBinding.transFctLty binding},
				     !newinvenv);
		    newstatenv := StaticEnv.bind(name, newbind, !newstatenv)
		end
	    | adjustBinding(_,STRbind(STRvar{name,access=PATH[lvar], binding})) =
		let val newlvar = lookup lvar
		    val newbind = STRbind(STRvar{name = name, access= PATH[newlvar],
						 binding = binding})
		 in newinvenv :=
		     InverseEnv.bind(newlvar,
				     {name=name,
				      pid=ModuleUtil.getStrStamp binding,
				      ty=TransBinding.transStrLty binding},
				     !newinvenv);
		    newstatenv := StaticEnv.bind (name, newbind, !newstatenv)
		end
	    | adjustBinding(n,b as SIGbind _) =
	        newstatenv := StaticEnv.bind(n,b,!newstatenv)
	    | adjustBinding _ = ErrorMsg.impossible "CompileUnit 2"

       in StaticEnv.app adjustBinding static;
	  {staticEnv={static= !newstatenv, inverse= !newinvenv},boundLvars=newlvars}
      end


 (* kosherModuleDecl:
  * Check that the declarations in the compilation unit are only
  * structures, signatures and functors.
  *)
  fun kosherModuleDecl decl =
      case decl
	of Absyn.FCTdec _ => ()
	 | Absyn.SIGdec _ => ()
	 | Absyn.STRdec _ => ()
	 | Absyn.MARKdec(dec,_,_) => kosherModuleDecl dec
	 | Absyn.SEQdec decs => app kosherModuleDecl decs
	 | _ => fail "expecting signatures/structures/functors"


  (* parsing *)

  fun elaborateUnit(source: Source.inputSource, {static=parseEnv,...}: staticEnv)
               : staticUnit =
      let val parser = Elaborate.parse (fn dec => dec) source

	  (* Loop on top-level declarations, allowing
	   * only signature, structure, and functor bindings.
	   *)
          fun loop(Elaborate.EOF, env, lvars) = (env, lvars)
	    | loop(Elaborate.ABORT, _,_) = fail "syntax error"
	    | loop(Elaborate.ERROR, _,_) = fail "syntax or semantic error"
	    | loop(Elaborate.PARSE(absyn,env'), env, lvars) =
		let val _ = kosherModuleDecl absyn
		    val newLvars = Linkage.getvars absyn
		    val newenv = StaticEnv.atop(env',env)
		    val fullEnv = StaticEnv.atop(newenv,parseEnv)
		 in loop(parser fullEnv, newenv, newLvars@lvars)
		end

	  val (env, lvars) =
		loop(parser(parseEnv),StaticEnv.empty,[])
		  handle Io x => (fail("unexpected: Io("^x^")"))
		       | exn as (Compile _) => raise exn
		       | exn => fail("compile-time exception: "
				     ^ System.exn_name exn)

       in {staticEnv=makeStaticEnv(StaticEnv.consolidate env), boundLvars=lvars}
      end  (* elaborateUnit *)

  fun parseUnit(source: Source.inputSource, {static=parseEnv,...}: staticEnv)
               : Ast.dec * staticEnv =
      let val parser = Parse.parse (fn dec => dec) source

	  (* Loop on top-level declarations, allowing
	   * only signature, structure, and functor bindings.
	   *)
          fun loop(Parse.EOF,ast, env) = (ast,env)
	    | loop(Parse.ABORT,_, _) = fail "syntax error"
	    | loop(Parse.ERROR,_, _) = fail "syntax or semantic error"
	    | loop(Parse.PARSE(ast',env'),ast, env) =
		let val newenv = StaticEnv.atop(env',env)
		    val newast = ast' :: ast
		    val fullEnv = StaticEnv.atop(newenv,parseEnv)
		 in loop(parser fullEnv, newast,newenv)
		end

	  val (ast,env) =
		loop(parser(parseEnv),[],StaticEnv.empty)
		  handle Io x => (fail("unexpected: Io("^x^")"))
		       | exn as (Compile _) => raise exn
		       | exn => fail("compile-time exception: "
				     ^ System.exn_name exn)

      in
      (Ast.SeqDec ast,makeStaticEnv(StaticEnv.consolidate env))
      end (* parseUnit *)


  (* compiling *)

  fun mkLambda (statenv, err, errMatch, anyErrors, dec) =
      (Translate.transDec statenv err errMatch dec)
      before
      (if !anyErrors then fail "error during translate" else ())

  fun close(lvars,t,lexp) =
      let val v = mkLvar()
	  fun doit(lv::rest,n) = APP(FN(lv,BOGUSty,doit(rest,n+1)), 
                                     SELECT(n, VAR v))
	    | doit(nil, _) = lexp
       in FN(v, t, doit(lvars, 0))
      end

 (* Close and fold down all the lambdas, generate code.
  * Also, use close to wrap with lambda abstractions binding free structure
  * references.
  *)
  fun compileLambda (openLambda,importLvars,imports,getty) =
    let val lt = RECORDty(map getty importLvars)
        val lambda' = close(importLvars, lt, openLambda)
	val finalLambda = Opt.closetop(lambda', !CoreInfo.corePath, getty)
	val mash = #1 o Convert.convert o Reorder.reorder o LambdaOpt.lambdaopt
	val executable = Machm.generate(mash finalLambda, NONE,
		    (fn _ => fn s => 
		      (app System.Print.say["Real constant out of range: ",s,"\n"];
		       fail "code generation failed")))
     in {executable=System.Code.mkCode executable, imports=imports}
    end

  fun loopAst (ast,env,src) = 
    let val (anyErrors,error,errorMatch) = 
	    case src
	      of NONE =>
		  let val anyErrors = ref false
		   in (anyErrors,
		       ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(), anyErrors),
		       fn _ => "Match error")
		  end
	       | SOME source =>
		  (#anyErrors source, ErrorMsg.error source,
		   ErrorMsg.matchErrorString source)
	val (absyn,newEnv) = 
          ElabStr.elaborateTop (ast,env,error,errorMatch,fn dec => dec)
	val _ = kosherModuleDecl absyn
	val newLvars = Linkage.getvars absyn
	val fullEnv = StaticEnv.atop(newEnv,env)
	val newLambda = mkLambda(fullEnv,error,errorMatch,anyErrors,absyn)
     in (newEnv,newLambda,newLvars)
    end

  fun loopSource (source,compEnv,_) =
      let val parser = Elaborate.parse (fn dec => dec) source
	  val initialLambda =
	    mkLambda(StaticEnv.empty, ErrorMsg.error source,
		     ErrorMsg.matchErrorString source, #anyErrors source,
		     Absyn.SEQdec nil)

	  (* Loop on top-level declarations, allowing
	   * only signature, structure, and functor bindings.
	   *)
          fun loop(Elaborate.EOF, env, lambda, lvars) = (env, lambda, lvars)
	    | loop(Elaborate.ABORT, _,_,_) = fail "syntax error"
	    | loop(Elaborate.ERROR, _,_,_) = fail "syntax or semantic error"
	    | loop(Elaborate.PARSE (absyn,env'), env, lambda, lvars) =
		let val _ = kosherModuleDecl absyn
		    val newLvars = Linkage.getvars absyn
		    val newenv = StaticEnv.atop(env',env)
		    val fullEnv = StaticEnv.atop(newenv,compEnv)
                    val _ = Index.report source (absyn, fullEnv)
		    val newLambda = 
			  mkLambda(fullEnv, ErrorMsg.error source,
                                   ErrorMsg.matchErrorString source,
				   #anyErrors source, absyn)
		 in loop(parser fullEnv, newenv, lambda o newLambda,
			 newLvars@lvars)
		end
      in loop(parser(compEnv),StaticEnv.empty,initialLambda,[]) end

  fun compileObj loop (object,{static=compEnv,inverse}: staticEnv,src) =
      let val (env, lambda, lvars) =
		loop(object,compEnv,src)
		  handle Io x => (fail("unexpected: Io("^x^")"))
		       | exn as (Compile _) => raise exn
		       | exn => fail("compile-time exception: "
					     ^ System.exn_name exn)

          val newenv = makeStaticEnv(StaticEnv.consolidate env)
	  val static = {staticEnv=newenv, boundLvars=lvars}
	  val openLambda = lambda(Lambda.RECORD(map Lambda.VAR lvars))
	  val freelvars = Opt.freevars openLambda
	  val invlook = InverseEnv.look inverse
	  fun mapfree([],x,y) = (x,y)
	    | mapfree(lv::r,x,y) =
	        mapfree((r, invlook lv :: x, lv :: y)
			handle InverseEnv.Unbound => (r,x,y))
	  val (importcomps,importlvars) = mapfree(freelvars,[],[])

          val getty = CompUtil.gengetty inverse

	  val code = compileLambda(openLambda,importlvars,importcomps,getty)
       in (static, code)
      end  (* compileObj *)

  fun compileAst (ast,env,src : Source.inputSource option) =
      let val ({staticEnv={static,inverse}, boundLvars},code) =
	    compileObj loopAst (ast,env,src)
       in ({staticEnv = {static = static, inverse = inverse},
	    boundLvars = boundLvars},
	   code)
      end

 (* compileUnit:
  * Compile a unit from an instream, producing a compUnit.
  *)

  fun compileUnit(source,env) = compileObj loopSource (source,env,NONE)


  (* executing *)

  fun executeCode({executable,imports}, environ: environment) : result =
      let val look_dyn = DynamicEnv.look (#dynamic environ)
	  val me: looker -> result -> result = System.Code.apply executable
	  fun nameOf s = 
	      (Symbol.nameSpaceToString(Symbol.nameSpace s))^" "^(Symbol.name s)
	  fun getImport {name,pid,ty} =
	      (case StaticEnv.look (#static environ, name)
		 of STRbind(STRvar{access=PATH[lv],binding,...}) =>
		      if (ModuleUtil.getStrStamp binding) = pid then (look_dyn lv)
		      else fail ("structure "^(nameOf name)^" has wrong stamp")
		  | FCTbind(FCTvar{access=PATH[lv],binding,...}) =>
		      if (ModuleUtil.getFctStamp binding) = pid then (look_dyn lv)
		      else fail ("functor "^(nameOf name)^" has wrong stamp")
		  | _ => ErrorMsg.impossible "ModuleComp.getImport"
	      ) handle StaticEnv.Unbound =>
			 fail((nameOf name)^
			      " is not found in the loading environment")
		     | _ => fail "error while executing module"
	  val importRecord = Vector.vector (map getImport imports) 
       in me look_dyn importRecord
	   handle exn => fail("uncaught exception "^ System.exn_name exn)
      end

 (* executeUnit:
  * Execute the code to produce the new values (result).
  * The lvars are bound to the new values to form a new
  * dynamic environment which is combined with the new static
  * environment to form the result environment (bindEnv).
  *)
  fun executeUnit(({staticEnv={static,inverse},boundLvars}, code): compUnit,
		  environ: environment)
        : environment =
      let val result = executeCode(code,environ)
       in (* PrintDec.printBindingTbl static; -- no printing! *)
	  bindEnv(static, inverse, boundLvars, result, emptyEnv)
      end

end (* functor CompileUnit *)


