
(* the structure compiler contains the functions constructing an
   ir-tree. The user of the code generator need not directly use
   the more detailed and less relevant low level facilities. *)

abstraction VaxBackend : INTERFACE =
  struct
    (* nodevalues contains the integer values of the ir tree nodes *)

    structure CGE = Vaxcg
    
    open CGE CGE.User

    type body = tree
    type stm = tree
    type exp = tree
    type test = tree
    type args = tree
      
    type unop = tree
    type binop = tree
    type relop = tree
      
    val s = initialized_support

(* binop: *)

    val bFPLUS  = tree(FPLUS,[],[],s())
    val bFMINUS  = tree(FMINUS,[],[],s())
    val bFMUL  = tree(FMUL,[],[],s())
    val bFDIV  = tree(FDIV,[],[],s())
    val bPLUS  = tree(PLUS,[],[],s())
    val bMINUS  = tree(MINUS,[],[],s())
    val bMUL  = tree(MUL,[],[],s())
    val bDIV  = tree(DIV,[],[],s())
    val bMOD  = tree(MOD,[],[],s())
    val bAND  = tree(AND,[],[],s())
    val bOR  = tree(OR,[],[],s())
    val bLSHIFT  = tree(LSHIFT,[],[],s())
    val bRSHIFT  = tree(RSHIFT,[],[],s())
    val bXOR  = tree(XOR,[],[],s())

    val rEQ  = tree(EQ,[],[],s())
    val rNEQ  = tree(NEQ,[],[],s())
    val rLT  = tree(LT,[],[],s())
    val rLEQ  = tree(LEQ,[],[],s())
    val rGT  = tree(GT,[],[],s())
    val rGEQ  = tree(GEQ,[],[],s())
    val rULT  = tree(ULT,[],[],s())
    val rULEQ  = tree(ULEQ,[],[],s())
    val rUGT  = tree(UGT,[],[],s())
    val rUGEQ  = tree(UGEQ,[],[],s())
    val rFEQ  = tree(FEQ,[],[],s())
    val rFNEQ  = tree(FNEQ,[],[],s())
    val rFLT  = tree(FLT,[],[],s())
    val rFLEQ  = tree(FLEQ,[],[],s())
    val rFGT  = tree(FGT,[],[],s())
    val rFGEQ  = tree(FGEQ,[],[],s())

    val uNEG  = tree(NEG,[],[],s())
    val uCOMP  = tree(COMP,[],[],s())
    val uFNEG  = tree(FNEG,[],[],s())
    val uCVTSU  = tree(CVTSU,[],[],s())
    val uCVTSS  = tree(CVTSS,[],[],s())
    val uCVTSF  = tree(CVTSF,[],[],s())
    val uCVTUU  = tree(CVTUU,[],[],s())
    val uCVTUS  = tree(CVTUS,[],[],s())
    val uCVTFS  = tree(CVTFS,[],[],s())
    val uCVTFF  = tree(CVTFF,[],[],s())

(* stm: *)

    fun nPROC (name, exp, stm) = tree(PROC,[exp,stm],[proclabel name],s())
    fun nSEQ (stm, stm') = tree(SEQ,[stm,stm'],[],s())
    fun nLABEL (labl) =  tree(LABEL,[],[label labl],s())
    fun nJUMP (exp) = tree(JUMP,[exp],[],s())
    fun nCJUMP (test, exp) = tree(CJUMP,[test,exp],[],s())
    fun nRETURN (exp) = tree(RETURN,[exp],[],s())

(* exp: *)

    fun nMEM (siz, exp) = tree(MEM, [exp],[size siz],s())
    fun nMOVE (siz, dest, exp) = tree(MOVE,[dest,exp],[size siz],s())
    fun nESEQ (siz, stm, exp) = tree(ESEQ,[stm,exp],[size siz],s())
    fun nBOOL (siz, test) = tree(BOOL,[test],[size siz],s())
    fun nNAME (siz, labl) = tree(NAME,[],[size siz,label labl],s())
    fun nCONST (siz, ival) = tree(CONST,[],[size siz,ivalue ival],s())
(*    fun nCONSTF (siz, fval) = tree(CONSTF,[],[size siz,fvalue fval],s())*)
    fun nALLOC (siz, tem, exp) = tree(ALLOC,[tem,exp],[size siz],s())
    fun nTEMP (siz, tem) = tree(TEMP,[],[size siz,temp tem],s())
    fun nCALL (siz, args, exp) = tree(CALL,[args,exp],[size siz],s())
    fun nIGNORE s = s

(* exp: with some not-so-nice simplifications *)

    fun opOF (tree(ope,_,_,_)) = ope
    fun isCONST (tree(CONST,_,_,_)) = true | isCONST _ = false
    fun isNAME (tree(NAME,_,_,_)) = true
      | isNAME (tree(OP,[bo,c1,c2],_,_)) =
	if opOF bo = PLUS andalso isNAME c1 andalso isCONST c2
	  then true else false
      | isNAME _ = false
    local
      fun attrOF (tree(_,_,attr,_)) = attr
      fun sizeOF' (size siz :: _) = siz
	| sizeOF' (h::t) = sizeOF' t
	| sizeOF' _ = 0
      fun ivalOF' (ivalue ival :: _) = ival
	| ivalOF' (h::t) = ivalOF' t
	| ivalOF' _ = 0
    in
      val sizeOF = sizeOF' o attrOF
      val ivalOF = ivalOF' o attrOF
    end

    fun commute operator = case operator of
      PLUS => true | MUL => true | XOR => true | OR => true | AND => true
    | _ => false

    fun assoc operator = case operator of
      PLUS => true | MUL => true | XOR => true | OR => true | AND => true
    | _ => false

    fun id0 operator = case operator of
      PLUS => true | MINUS => true | XOR => true | OR => true
    | _ => false

    fun id1 operator = case operator of
      MUL => true | DIV => true | _ => false

    fun swap operator =
      case operator of
	EQ => EQ | NEQ => NEQ | GT => LT | GEQ => LEQ | LT => GT | LEQ => GEQ
      | UGT => ULT | UGEQ => ULEQ | ULT => UGT | ULEQ => UGEQ | FEQ => FEQ
      | FNEQ => FNEQ | FGT => FLT | FGEQ => FLEQ | FLT => FGT | FLEQ => FGEQ
      | other => other

    fun S1 (arg as (siz,binop,exp,exp')) =
      if commutative (opOF binop) andalso isCONST exp
	then (siz,binop,exp',exp)
      else arg

    and S2 (arg as (siz,binop,exp,exp')) =
      if commutative (opOF binop) andalso isNAME exp
	then (siz,binop,exp',exp)
      else arg

    and S6 (arg as (siz,binop,tree(OP,[binop',c1,c2],r1,r2),exp')) =
      if opOF binop = MUL andalso opOF binop' = PLUS
	andalso isCONST c2 andalso isCONST exp'
	then (siz,
	      bPLUS,
	      tree(OP,[bMUL,c1,exp'],r1,r2),
	      nCONST(siz,(ivalOF c2)*(ivalOF exp')))
      else arg
      | S6 arg = arg
	
    and S7 (arg as (siz,binop,exp,exp')) =
      if opOF binop = MINUS andalso isCONST exp'
	then (siz,bPLUS,exp,nCONST(siz,0-(ivalOF exp')))
      else arg

    and S8 (arg as (siz,binop,tree(OP,[binop',c1,c2],r1,r2),exp')) =
      if (assoc o opOF) binop andalso opOF binop = opOF binop' andalso
	isCONST exp' andalso isCONST c2
	then (siz,binop,c1,S3(siz,binop,c2,exp'))
      else arg
      | S8 arg = arg

    and S4 (arg as (siz,binop,exp,exp')) =
      if (id0 o opOF) binop andalso isCONST exp' andalso ivalOF exp' = 0
	then exp
      else (STOP o S8 o S7 o S6) arg

    and S5 (arg as (siz,binop,exp,exp')) =
      if (id1 o opOF) binop andalso isCONST exp' andalso ivalOF exp' = 1
	then exp
      else S4 arg

    and S3 (arg as (siz,binop,exp,exp')) =
      let open Bits
      in
	if isCONST exp andalso isCONST exp'
	  then
	    case opOF binop of
	      PLUS => nCONST(siz,(ivalOF exp)+(ivalOF exp'))
	    | MINUS => nCONST(siz,(ivalOF exp)-(ivalOF exp'))
	    | MUL => nCONST(siz,(ivalOF exp)*(ivalOF exp'))
	    | DIV => nCONST(siz,(ivalOF exp)div(ivalOF exp'))
	    | MOD => nCONST(siz,(ivalOF exp)mod(ivalOF exp'))
	    | AND => nCONST(siz,andb((ivalOF exp),(ivalOF exp')))
	    | OR => nCONST(siz,orb((ivalOF exp),(ivalOF exp')))
	    | XOR => nCONST(siz,xorb((ivalOF exp),(ivalOF exp')))
	    | LSHIFT => nCONST(siz,lshift((ivalOF exp),(ivalOF exp')))
	    | RSHIFT => nCONST(siz,rshift((ivalOF exp),(ivalOF exp')))
	    | _ => raise CodeGeneration "Unknown operator"
	else (S5 o S2 o S1) arg
      end

    and STOP (siz,binop,exp,exp') = tree(OP,[binop,exp,exp'],[size siz],s())

    val nOP = S3

    fun nUNOP (siz, unop, exp) =
      case exp of
	tree(CONST,_,_,_) =>
	  nCONST(siz,
		 case opOF unop of
		   NEG => 0-(ivalOF exp)
		 (* | COMP => as
		 | FNEG => a
		 | CVTSU => ivalOF exp
		 | CVTSS => ivalOF exp
		 | CVTSF => 
		 | CVTUU => a
		 | CVTUS => a
		 | CVTFS => a
		 | CVTFF => a *)
		 | _ => raise CodeGeneration "cannot simplify unary operator")
      | _ => tree(UNOP,[unop,exp],[size siz],s())

    fun nOPr (relop, exp, exp') =
      if isCONST exp
	then tree(OP,[tree((swap o opOF) relop,[],[],s()),exp',exp],[],s())
      else tree(OP,[relop,exp,exp'],[],s())

(* test: *)

    fun nCAND (test, test') = tree(CAND,[test,test'],[],s())
    fun nNOT (test) = tree(NOT,[test],[],s())

(* args: *)

    fun nARG (exp, args) = tree(ARG, [exp,args],[],s())
    fun nNOARGS () = tree(NOARGS,[],[],s())

(* registers: *)

    val FP = FPregister
    val AP = APregister

(* main *)
      
    fun compile t = (output (std_out,".text\n");translate t; ())

end;
