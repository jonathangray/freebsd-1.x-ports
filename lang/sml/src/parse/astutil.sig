(* Copyright 1992 by AT&T Bell Laboratories *)

(* Various functions used by the parser to build the AST of the
   term it reads. 
   Functions can be splitted  in four classes:
    - handling the infixes environments
    - checking coherence of definitions
    - building declarations
    - resolving the parsing of infixes
   File ends with the declaration of a lot of useful constant expressions.
 *)

signature ASTUTIL =
  sig

    type rawclause
    type rawrvb
    type 'a precStack
    type parseEnv

    (* CHECKING COHERENCE OF DEFS *)
    val checkUniq :
	  ErrorMsg.complainer * string -> Symbol.symbol list -> unit
    val checkFB :
	  rawclause list * ErrorMsg.complainer -> rawclause list
    val checkFix :
	  int * ErrorMsg.complainer -> int
    val makecl : 
      (Ast.pat * Fixity.fixity * ErrorMsg.complainer) list *
      (Ast.pat * Fixity.fixity * ErrorMsg.complainer) list
      -> Symbol.symbol * Ast.pat list

    (* BUILDS VARIOUS CONSTRUCTIONS *)
    val makeFIXdec :
	  Fixity.fixity * Symbol.symbol list -> parseEnv
	  -> Ast.dec * (parseEnv -> parseEnv)
    val makeFUNdec : 
	  (parseEnv -> (rawclause list * int * int)list) * 'a -> parseEnv
	  -> Ast.dec * (parseEnv -> parseEnv)
    val makeLETstr : 
	  (parseEnv -> Ast.dec * (parseEnv -> parseEnv))
	  * (parseEnv -> Ast.strexp)
	  -> parseEnv -> Ast.strexp
    val makeLETfct : 
	  (parseEnv -> Ast.dec * (parseEnv -> parseEnv))
	  * (parseEnv * Ast.fsigexp option -> Ast.fctexp)
	  -> parseEnv -> Ast.fsigexp option -> Ast.fctexp
    val makeLOCALdec : 
	  (parseEnv -> Ast.dec * (parseEnv -> parseEnv)) 
	  * (parseEnv -> Ast.dec * (parseEnv -> parseEnv))
	  -> parseEnv -> Ast.dec * (parseEnv -> parseEnv)
    val makeSEQdec :
      (parseEnv -> Ast.dec * (parseEnv -> parseEnv)) * 
      (parseEnv -> Ast.dec * (parseEnv -> parseEnv)) -> parseEnv 
      -> Ast.dec * (parseEnv -> parseEnv)
    val makeVALRECdec :
      (parseEnv -> rawrvb list) * 'a -> parseEnv -> Ast.dec * parseEnv

    val toplevelexp : 'a * ('a -> Ast.exp) -> Ast.dec * parseEnv


    (* PARSE PATTERNS *)
    val lookFIX : parseEnv * Symbol.symbol -> Fixity.fixity
    val make_app_pat : 
      (Ast.pat * Fixity.fixity * ErrorMsg.complainer) list
      -> Ast.pat
    val layered :
	  Ast.pat * Ast.pat * ErrorMsg.complainer -> Ast.pat

    (* PARSE EXPRESSION *)
    val exp_start :
	  Ast.exp * Fixity.fixity * ErrorMsg.complainer
	  -> Ast.exp precStack
    val exp_parse : 
	  Ast.exp precStack * Ast.exp * Fixity.fixity * ErrorMsg.complainer
	  -> Ast.exp precStack
    val exp_finish :
	  Ast.exp precStack * ErrorMsg.complainer -> Ast.exp

    (* SYMBOLS *)
    val arrowTycon : Symbol.symbol
    val bogusID : Symbol.symbol
    val exnID : Symbol.symbol
    val symArg : Symbol.symbol
    val itsym : Symbol.symbol list

    (* DATA CONSTRUCTOR SYMBOLS *)
    val trueDcon : Symbol.symbol list
    val falseDcon : Symbol.symbol list
    val nilDcon : Symbol.symbol list
    val consDcon : Symbol.symbol list

    (* EXPRESSIONS (CONSTANTS) *)
    val trueExp : Ast.exp
    val falseExp : Ast.exp
    val unitExp : Ast.exp
    val nilExp : Ast.exp
    val consExp : Ast.exp

    (* PATTERNS *)
    val truePat : Ast.pat
    val falsePat : Ast.pat
    val unitPat : Ast.pat
    val nilPat : Ast.pat
    val consPat : Ast.pat -> Ast.pat

    (* LIST BUILDERS *)
    val ListExp : Ast.exp list -> Ast.exp
    val ListPat : Ast.pat list -> Ast.pat

    (* QUOTES *)
    val QuoteExp : string -> Ast.exp
    val AntiquoteExp : Ast.exp -> Ast.exp

  end

