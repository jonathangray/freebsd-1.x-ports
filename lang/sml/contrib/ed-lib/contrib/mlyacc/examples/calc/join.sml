structure Calc : sig
	           val parse : unit -> unit
                 end = 
 struct
  structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
  structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
  structure CalcParser = Join(structure LrParser = LrParser
		            structure ParserData = CalcLrVals.ParserData
		            structure Lex = CalcLex)

  val invoke = fn lexstream =>
    let val print_error = fn (s,i:int,_) =>
         output(std_out,"Error, line " ^ (Int.string i) ^ ", " ^ s ^ "\n")
    in CalcParser.parse(0,lexstream,print_error,())
    end

  val parse = fn () => 
    let val lexer = CalcParser.makeLexer (fn _ => Instream.inputLine std_in)
        val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
	val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
	   let val (result,lexer) = invoke lexer
	       val (nextToken,lexer) = CalcParser.Stream.get lexer
	       val _ = case result
		  of Some r => output(std_out,
                                   ("result = " ^ (Int.string r) ^ "\n"))
		   | None => ()
	   in if CalcParser.sameToken(nextToken,dummyEOF) then ()
	      else loop lexer
	   end
     in loop lexer
     end
end;
