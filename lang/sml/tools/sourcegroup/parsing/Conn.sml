(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

functor ConnFun
  (structure Interface :INTERFACE
   structure Parser :PARSER
     sharing type Parser.pos = Interface.pos
     sharing type Parser.arg = unit
   structure Tokens :Conn_TOKENS
     sharing type Tokens.token = Parser.Token.token
     sharing type Tokens.svalue = Parser.svalue
   structure AbSyn :ABSYN
     sharing type Parser.result = AbSyn.ast
  ) :CONN = struct

structure AbSyn = AbSyn

fun reportError (msg:string, lineNum:Parser.pos, _:Parser.pos) =
  print ("? Connections: (line "^(Interface.makeString lineNum)^") "^msg^"\n")

fun parse (verbose:bool) (filename :string) :AbSyn.ast =
  let val source'stream = open_in filename
      val lexer = Parser.makeLexer (inputc source'stream)
      val lookahead = 30
      val _ = Interface.initLine()
      val (ast, _) =
            (Parser.parse (lookahead, lexer, reportError, ()))
               handle e => (close_in source'stream; raise e)
  in
    close_in source'stream;
    if verbose then AbSyn.printast std_out ast else ();
    ast
  end

end
