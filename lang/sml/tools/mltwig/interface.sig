
signature INTERFACE =
  sig
    type body
    type stm
    type exp
    type test
    type args
      
    type binop
    type unop
    type relop

    type Reg

    exception CodeGeneration of string
    exception NoCover
      
    val nALLOC : int * exp * exp -> exp
    val nARG : exp * args -> args
    val nBOOL : int * test -> exp
    val nCALL : int * args * exp -> exp
    val nCAND : test * test -> test
    val nCJUMP : test * exp -> stm
    val nCONST : int * int -> exp
    val nESEQ : int * stm * exp -> exp
    val nJUMP : exp -> stm
    val nLABEL : string -> stm
    val nMEM : int * exp -> exp
    val nMOVE : int * exp * exp -> exp
    val nNAME : int * string -> exp
    val nNOARGS : unit -> args
    val nNOT : test -> test
    val nOP : int * binop * exp * exp -> exp
    val nOPr : relop * exp * exp -> test
    val nPROC : string * exp * stm -> body
    val nSEQ : stm * stm -> stm
    val nRETURN : exp -> stm
    val nTEMP : int * Reg -> exp
    val nUNOP : int * unop * exp -> exp
    val nIGNORE : exp -> stm
      
    val FP : Reg
    val AP : Reg

    val bFPLUS : binop
    val bFMINUS : binop
    val bFMUL : binop
    val bFDIV : binop
    val bPLUS : binop
    val bMINUS : binop
    val bMUL : binop
    val bDIV : binop
    val bMOD : binop
    val bAND : binop
    val bOR : binop
    val bLSHIFT : binop
    val bRSHIFT : binop
    val bXOR : binop
    val rEQ : relop
    val rNEQ : relop
    val rLT : relop
    val rLEQ : relop
    val rGT : relop
    val rGEQ : relop
    val rULT : relop
    val rULEQ : relop
    val rUGT : relop
    val rUGEQ : relop
    val rFEQ : relop
    val rFNEQ : relop
    val rFLT : relop
    val rFLEQ : relop
    val rFGT : relop
    val rFGEQ : relop
    val uNEG : unop
    val uCOMP : unop
    val uFNEG : unop
    val uCVTSU : unop
    val uCVTSS : unop
    val uCVTSF : unop
    val uCVTUU : unop
    val uCVTUS : unop
    val uCVTFS : unop
    val uCVTFF : unop

    val compile : body -> unit
      
  end;
