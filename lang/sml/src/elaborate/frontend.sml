(* Copyright 1989 by AT&T Bell Laboratories *)

functor FrontEnd(type output 
		 val elaborate:Ast.dec * Modules.env * Modules.env *
			(int * int -> ErrorMsg.complainer) *
			(int * int -> string) * (output -> output) 
			-> output * Modules.env) :
  sig
    datatype parseResult
      = EOF   (* end of file reached *)
      | ERROR (* parsed successfully, but with syntactic or semantic errors *)
      | ABORT (* could not even parse to end of declaration *)
      | PARSE of output * Modules.env
    val parse : (output -> output) -> Source.inputSource -> 
		Modules.env -> parseResult
  end
 =
struct 
  structure MLLrVals = MLLrValsFun(structure Token = LrParser.Token)
  structure Lex = MLLexFun(structure Tokens = MLLrVals.Tokens)
  structure MLP = JoinWithArg(structure ParserData = MLLrVals.ParserData
			      structure Lex=Lex
			      structure LrParser = LrParser)
  structure Absyn = Absyn

  open ErrorMsg

  (* the following two functions are also defined in build/computil.sml *)
  fun debugmsg  (msg : string) =
      let val printit = !System.Control.debugging
      in  if printit then app System.Print.say[msg, "\n"]
	  else ();
	  printit
      end

  fun timemsg (s : string) =
      let val printit = !System.Control.timings
       in if printit then (app System.Print.say[s, "\n"]; System.Print.flush()) 
	             else ();
	  printit
      end

  datatype parseResult
    = EOF   (* end of file reached *)
    | ERROR (* parsed successfully, but with syntactic or semantic errors *)
    | ABORT (* could not even parse to end of declaration *)
    | PARSE of output * Modules.env

  val dummyEOF = MLLrVals.Tokens.EOF(0,0)
  val dummySEMI = MLLrVals.Tokens.SEMICOLON(0,0)

  fun parse (transform:output -> output) 
            (source as {sourceStream,errConsumer,interactive,
			linePos,lineNum,anyErrors,...}: Source.inputSource) =
      let val lastLineNum = ref(!lineNum-1)

	  val err = ErrorMsg.error source
	  val complainMatch = ErrorMsg.matchErrorString source

	  fun parseerror(s,p1,p2) = err (p1,p2) COMPLAIN s nullErrorBody

	  val lexarg = {comLevel = ref 0,
			lineNum = lineNum,
			linePos = linePos,
			charlist = ref (nil : string list),
			stringstart = ref 0,
			err = err,
                        brack_stack = ref (nil: int ref list)}

	  val doprompt = ref true
	  val prompt = ref (!System.Control.primaryPrompt)

	  val inputc_sourceStream = inputc sourceStream

	  exception AbortLex
	  fun getline k =
	      (if !doprompt
	       then (if !anyErrors then raise AbortLex else ();
		     System.Print.say
		       (if !(#comLevel lexarg) > 0
			   orelse !(#charlist lexarg) <> nil
			then !System.Control.secondaryPrompt
			else !prompt);
		     System.Print.flush ();
		     doprompt := false)
	       else ();
	       let val s = inputc_sourceStream k
	        in doprompt := (ordof(s,size s - 1)=ord("\n") handle Ord => false);
		   s
	       end)

	  val lexer = Lex.makeLexer
	                (if interactive
			 then getline
			 else inputc_sourceStream)
		        lexarg
	  val lexer' = ref(LrParser.Stream.streamify lexer)
	  val lookahead = if interactive then 0 else 30

	  fun oneparse env =
	      let val _ = prompt := !System.Control.primaryPrompt
		  val (nextToken,rest) = LrParser.Stream.get(!lexer') 
	       in if interactive then linePos := [hd(!linePos)] else ();
		  if MLP.sameToken(nextToken,dummySEMI) 
		  then (lexer' := rest; oneparse env)
		  else if MLP.sameToken(nextToken,dummyEOF)
		  then EOF
		  else let val _ = prompt := !System.Control.secondaryPrompt;
			   open System.Timer
			   val t1 = start_timer()
			   val (f, lexer'') =
				 MLP.parse(lookahead,!lexer',parseerror,err)
			   val t2 = check_timer t1
			   val lines = !lineNum - !lastLineNum
			   val _ = System.Stats.lines :=
			             !System.Stats.lines + lines
			   val _ = timemsg("parse, " ^ 
				       Integer.makestring lines
				       ^ " lines, " ^ makestring t2 ^"s")
				   orelse debugmsg "parse"
			   val _ = lexer' := lexer''
			   val (ast,envfix) = f env
			   val (absyn,envdec) =
			     elaborate (ast,env,envfix,err,
					complainMatch,transform)
			   val result = (absyn,envdec)
			   val t3 = check_timer t1
			in System.Stats.update(System.Stats.parse,t3);
			   timemsg("semantics, "^makestring(sub_time(t3,t2))
				   ^"s")
			       orelse debugmsg "semantics";
			   if !anyErrors then ERROR else PARSE result
		       end 
	      end handle LrParser.ParseError => ABORT
		       | AbortLex => ABORT
              (* oneparse *)

       in fn env =>
	    (lastLineNum := !lineNum; anyErrors := false; oneparse env)
      end

end; (* functor FrontEnd *)

structure Elaborate = FrontEnd(type output = Absyn.dec
			       fun elaborate (ast,env,fixenv,err,errMatch,
					      transform) =
				 let val (decl,deltaenv) = 
				       ElabStr.elaborateTop 
				       (ast,env,err,errMatch,
					transform)
				 in (decl,Env.atop(deltaenv,fixenv)) end)

structure Parse = FrontEnd(type output = Ast.dec 
			   fun elaborate (ast,env,delta,_,_,_) = (ast,delta))

