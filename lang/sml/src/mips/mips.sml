(* mips.sml
 *
 * Copyright (c) 1992 by AT&T Bell Laboratories
 *)

functor MipsCM(structure C: CODER and E: ENDIAN
	         sharing type C.instruction = MipsInstrSet.instruction
		     and type C.sdi = MipsInstrSet.sdi) : CMACHINE =
struct

  structure M = MipsInstrSet
  open M
  type EA = C.label M.EA

  val error = ErrorMsg.impossible

  exception BadReal 	= C.BadReal
  val align 		= fn () => ()
  val mark 		= C.mark
  val emitlong 		= C.emitLong
  val realconst 	= C.emitReal
  val emitstring 	= C.emitString
  val newlabel 		= M.ImmedLab o C.newLabel
  val immed       	= M.Immed 
  val emitSDI		= C.emitSDI
  val emit		= C.emit

  fun emitlab(k,ImmedLab lab) = C.emitLabel(lab,k)
    | emitlab _ = error "MipsCM.emitlab"

  fun define(ImmedLab lab) = C.define lab
    | define _ = error "MipsCM.define"


  (* Register Map
     Reg   gc   desc
     -------------------------------------
     0	   n	zero
     1     n    temporary reg
     2	   y	standard arg
     3	   y	standard continuation
     4	   y    standard closure
     5	   y	standard link
     6-18  y    misc regs
     19    n    limit reg
     20    n    var pointer
     21    n    temporary reg & heapExhausted reg.
     22    n    store pointer
     23    n    allocation pointer
     24	   n    base reg
     25    n    temporary reg & maskReg
     26-27 n	reserved for OS kernel
     28	   n	C global pointer
     29	   n	C stack pointer
     31	   n	temporary reg & gclink register
  *)
  val varptr_indexable	 	   = true
  val standardlink                 = Direct(Reg 5)
  val standardarg		   = Direct(Reg 2)
  val standardcont		   = Direct(Reg 3)
  val standardclosure		   = Direct(Reg 4)
  val miscregs 			   = map (Direct o Reg) 
      				         [6,7,8,9,10,11,12,13,14,15,16,17,18]

  val limit as Direct limit'   	   = Direct(M.limitReg)
  val varptr 			   = Direct(Reg 20)
  val stackptr'                     =      (Reg 29)

  val storeptr as Direct storeptr' = Direct(Reg 22)
  val dataptr as Direct dataptr'   = Direct(M.allocReg)

  val exnptr	  		   = Direct(M.exnptrReg)

  val floatregs: EA list	   = map (Direct o Freg)
      					 [0,2,4,6,8,10,12,14,16,18,30]
  val savedfpregs: EA list	   = map (Direct o Freg) [20,22,24,26,28]

  val arithtemps: EA list	   = []

  local
      exception NoTmpRegs
      val tmpRegs	= [M.heapExhaustedReg,M.maskReg,M.linkReg,Reg 1]
      val queue         = ref tmpRegs
  in  
      fun getTmpReg()    = case !queue
	                   of hd :: rest => (queue := rest; hd)
			    | _ => raise NoTmpRegs
      fun freeTmpReg reg = queue := !queue @ [reg]
  end


  fun emitBRANCH(cond,rs,rt,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in 
	  emitSDI(M.BRANCH(cond,rs,rt,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end

  fun emitBRANCH_COP1(cond,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in
	  emitSDI(M.BRANCH_COP1(cond,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end


  datatype immedSize = IMMED16 | IMMED32

  fun immed_size n = if (~32764<=n) andalso (n<=32764) 
		     then IMMED16 
		     else IMMED32
  fun load_immed(n,r) =  
      case (immed_size n)
	of IMMED16 => emit(M.ADD(r,Reg 0,Immed16Op n))
	 | IMMED32 => let val (hi,lo) = M.split n
		      in  emit (M.LUI(r,Immed16Off hi));
			  emit (M.ADD(r,r,Immed16Op lo))
		      end

  fun do_immed_arith(instr,rt,rs,n) =
      case (immed_size n)
	of IMMED16 => emit(instr(rt,rs,Immed16Op n))
	 | IMMED32 => let 
			  val (hi,lo) = M.split n
			  val tmpR = getTmpReg()
		      in 
			  emit (M.LUI(tmpR,Immed16Off hi));
			  emit (M.ADD(tmpR,tmpR,Immed16Op lo));
			  emit (instr(rt,rs,RegOp tmpR));
			  freeTmpReg tmpR
		      end

  fun do_immed_mem(instr,rt,base,n) =
      case (immed_size n)
	of IMMED16 => emit(instr(rt,base,Immed16Off n))
	 | IMMED32 => let 
			  val (hi,lo) = M.split n
			  val tmpR = getTmpReg()
		      in 
			  emit (M.LUI(tmpR,Immed16Off hi));
			  emit (M.ADD(tmpR,tmpR,RegOp base));
			  emit (instr(rt,tmpR,Immed16Off lo));
			  freeTmpReg tmpR
		      end


 (*
  * move(a,b) means b <- a
  *)
  val Reg0 = Reg 0
  val RegOp0 = RegOp(Reg 0)

  fun move(Direct a, Direct b) =
        (case (reg_rep a, reg_rep b)
          of (Freg' _, Freg' _) => emit(M.MOV_DOUBLE(b,a))
 	   | (Freg' _, _) => error "MipsCM.move: destination not a float reg"
	   | (_, Freg' _) => error "MipsCM.move: source not a float reg"
	   | (Reg' a', Reg' b') => if a'=b' then ()
		                   else emit(M.ADD(b,a,RegOp0)))
    | move(ImmedLab lab, Direct dst) = emitSDI(LOADADDR(dst,lab,0))
    | move(Immed n, Direct dst) = load_immed(n,dst)
    | move _ = error "MipsCM.move"

  fun jmp (Direct r)     = emit(M.JUMP r)
    | jmp (ImmedLab lab) = emitBRANCH(true,Reg0,Reg0,lab)
    | jmp _              = error "MipsCM.jmp: bad target"


 (*
  * Note:mipsdepend will ensure that nothing generated here
  * 	gets reordered.
  *)
  fun testLimit() = emit(M.SLT(M.heapExhaustedReg,dataptr',RegOp(limit')))

  val startgc_offset = 4

  fun checkLimit(max_allocation, lab, mask) =
      (* NOTE: THIS CODE USES TEMP REGS BY ALIASES.
       Thus it is important that none of the emitted pseudo-instructions
       below uses getTmpReg(), directly or indirectly. *)
    let val lab' = C.newLabel()
     in if max_allocation > 4096
        then (do_immed_arith(M.ADD,M.heapExhaustedReg,dataptr',
			     max_allocation - 4096);
              emit(M.SLT(M.heapExhaustedReg,M.heapExhaustedReg,RegOp(limit'))))
        else ();
        emitBRANCH(false,M.heapExhaustedReg, Reg0, lab');
	do_immed_mem(M.LW,M.heapExhaustedReg,stackptr',startgc_offset);
	move(mask, Direct M.maskReg);
	move(lab, Direct M.linkReg);
        emit(M.JUMP M.heapExhaustedReg);
        C.define lab'
    end

  fun beginStdFn(ImmedLab lab, Direct reg) = emitSDI(M.SETBASEADDR(lab,reg))

 (*
  * jmpindexb(x,y) means pc <- (x+y) 
  *)
  fun jmpindexb(ImmedLab lab,Direct y) = 
      let val tmpR = getTmpReg()
      in 
	emitSDI(LOADADDR(tmpR,lab,0));
	emit(M.ADD(tmpR,y,RegOp tmpR));
	emit(M.JUMP tmpR);
	freeTmpReg tmpR
      end
    | jmpindexb _ = error "MipsCM.jmpindexb"


 (* should be rewritten to use all the temp registers *)
  fun record(vl, Direct z) = let
        open CPS
	val len = List.length vl
	fun f(_,i,nil) = ()
	  | f((t1,t2),i,(Direct r, SELp(j,p))::rest) = 
	       (* follow ptrs to get the item  *)
		(emit(M.LW(t1,r,Immed16Off(j*4))); f((t2,t1),i,(Direct t1,p)::rest))
	  | f(t,i,(Direct r,OFFp 0)::rest) = 
	       (*  simple store, last first  *) 
		(emit(M.SW(r,dataptr',Immed16Off(i*4)));  f(t,i-1,rest))
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
		(emit(M.ADD(t1,r,Immed16Op(4*j))); 
		 f((t2,t1),i,(Direct t1,OFFp 0)::rest))
	  | f((t1,t2),i,(ea,p)::rest) =
	       (* convert to register-based  *)
		(move(ea,Direct t1);  f((t2,t1),i,(Direct t1,p)::rest))
	val tmpR1 = getTmpReg()
	val tmpR2 = getTmpReg()
      in 
       (* store first word in 0(dataptr') *)
	f((tmpR1,tmpR2),len-1,rev vl); 
	freeTmpReg tmpR1;
	freeTmpReg tmpR2;
	emit (M.ADD(z,dataptr',Immed16Op 4));
	do_immed_arith(M.ADD,dataptr',dataptr',4*len)
      end
    | record _ = error "MipsCM.record: result not a register"

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(z-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, _) = record ([
	    (Immed(System.Tags.make_desc(3, System.Tags.tag_record)), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)


  fun select(i,Direct v',Direct w) = do_immed_mem(M.LW,w,v',i*4)
    | select(i,ImmedLab lab,Direct w) = emitSDI(LOAD(w,lab,i*4))
    | select _ = error "MipsCM.select: bad dst"


  fun offset(i,v,Direct w) =
      (case v
	 of Direct v'    => do_immed_arith(M.ADD,w,v',i*4)
	  | ImmedLab lab => let val tmpR = getTmpReg()
			    in
				emitSDI(LOADADDR(tmpR,lab,0));
				do_immed_arith(M.ADD,w,tmpR,i*4);
				freeTmpReg tmpR
			    end
	  | _ 	       => error "MipsCM.offset: bad src")
    | offset _ = error "MipsCM.offset: bad dst"


 (* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], 
  *	where y is not x or z 
  *)
  fun fetchindexb(Direct x,Direct y,Immed indx)  = do_immed_mem(M.LBU,y,x,indx)
    | fetchindexb(Direct x,Direct y,Direct indx) = let
          val tmpR = getTmpReg()
      in 
	  emit (M.ADD(tmpR,indx,RegOp x));
	  emit (M.LBU(y,tmpR,Immed16Off 0));
	  freeTmpReg tmpR
      end
    | fetchindexb _ = error "MipsCM.fetchindexb"


 (* 
  * storeindexb(x,y,z) stores a byte: mem[y+z] <- x. 
  *)
  fun storeindexb(Immed xi,y,z) = 
      let val tmpR = getTmpReg()
      in
	  do_immed_arith(M.ADD,tmpR,Reg0,xi);  
	  storeindexb(Direct tmpR,y,z);
	  freeTmpReg tmpR
      end
    | storeindexb(Direct x,Direct y,Direct indx) =
      let val tmpR = getTmpReg()
      in 
	  emit (M.ADD(tmpR,y,RegOp indx));
	  emit (M.SB(x,tmpR,Immed16Off 0));
	  freeTmpReg tmpR
      end
    | storeindexb(Direct x,Direct y,Immed indx) = do_immed_mem(M.SB,x,y,indx)
    | storeindexb _ = error "MipsCM.storeindexb" 


 (* 
  * fetchindexl(x,y,z) fetches a word:   y <- mem[x+2*(z-1)] 
  *)
  fun fetchindexl(x,Direct y,Direct z') = 
      let val tmpR = getTmpReg()
      in
	  emit(M.SLL(tmpR,z',Int5 1));
	  (case x 
	       of Direct x' => (emit (M.ADD(tmpR,x',RegOp tmpR));
				emit (M.LW(y,tmpR,Immed16Off ~2)))
	     | Immed n      => do_immed_mem(M.LW,y,tmpR,n-2)
	     | ImmedLab lab => 
		   let val tmpR2 = getTmpReg()
		   in
		       emitSDI(M.LOADADDR(tmpR2,lab,0));
		       emit(M.ADD(tmpR,tmpR,RegOp tmpR2));
		       freeTmpReg tmpR2;
		       emit(M.LW(y,tmpR,Immed16Off ~2))
		   end);
	       freeTmpReg tmpR
      end
    | fetchindexl(x,Direct y,Immed z') =  
      (case x
	 of Direct x'    => do_immed_mem(M.LW,y,x',2*(z'-1))
	  | Immed n      => do_immed_mem(M.LW,y,Reg0,n+2*(z'-1))
	  | ImmedLab lab => emitSDI(LOAD(y,lab,2*(z'-1))))
    | fetchindexl _ = error "MipsCM.fetchindexl"


 (* 
  * storeindexl(x,y,z) stores a word:    mem[y+2*(z-1)] <- x 
  *)
  fun storeindexl(Direct x,Direct y,Direct z) = 
      let val tmpR = getTmpReg()
      in 
	  emit (M.SLL(tmpR,z,Int5 1));
	  emit (M.ADD(tmpR,tmpR,RegOp y));
	  emit (M.SW(x,tmpR,Immed16Off ~2));
	  freeTmpReg tmpR
      end
    | storeindexl(Direct x,Direct y,Immed zi) = do_immed_mem(M.SW,x,y,2*(zi-1))
    | storeindexl(Immed xi,y,z) =  let val tmpR = getTmpReg()
				   in
				       load_immed(xi,tmpR); 
				       storeindexl(Direct tmpR,y,z);
				       freeTmpReg tmpR
				   end
    | storeindexl(ImmedLab lab,y,z) = let val tmpR = getTmpReg()
				      in
					  emitSDI(M.LOADADDR(tmpR,lab,0));
					  storeindexl(Direct tmpR,y,z);
					  freeTmpReg tmpR
				      end
    | storeindexl(Direct x,ImmedLab lab,Immed zi) = 
      let val tmpR = getTmpReg()
      in 
	  emitSDI(M.LOADADDR(tmpR,lab,0));  
	  do_immed_mem(M.SW,x,tmpR,2*(zi-1));
	  freeTmpReg tmpR
      end
    | storeindexl _ = error "MipsCM.storeindexl: bad args"


 (*
  * three - can *only* be used for commutative operators
  *)
  fun three f (Direct x',Direct y', ea) = 
      (case ea
	 of Immed zi     => do_immed_arith(f,x',y',zi)
	  | Direct z'    => emit(f(x',y',RegOp z'))
	  | ImmedLab lab => let val tmpR = getTmpReg()
			    in
				emitSDI(M.LOADADDR(tmpR,lab,0));  
				emit(f(x',y',RegOp tmpR));
				freeTmpReg tmpR
			    end)
    | three f (Direct x', ea, Direct z') = three f (Direct x',Direct z',ea)
    | three _ _ = error "MipsCM.three: bad args"

  fun add(x,y,z) 	= three M.ADD (z,x,y)
  fun orb(x,y,z) 	= three M.OR  (z,x,y) 
  fun andb(x,y,z)	= three M.AND (z,x,y)
  fun xorb(x,y,z)	= three M.XOR (z,x,y)

 (* Subtraction may appear a bit odd. 
  * The MIPS machine instruction and  MIPSCODER.sub both subtract 
  * their second operand from their first operand.
  * The CMACHINE.sub subtracts the first operand from the second.
  * This will certainly lead to endless confusion.
  *
  * sub(a,b,c) mean c <- b-a
  *)
  fun op sub(Immed xi,y,z) 	      = add(y,Immed (~xi),z)
    | op sub(Direct x,Direct y,Direct z)= emit(M.SUB(z,y,x))
    | op sub(x,Immed 0,dest)            = op sub(x, Direct(Reg0), dest)
    | op sub(x,Immed k,dest)            = let val tmpR = getTmpReg()
					  in
					      do_immed_arith(M.ADD,tmpR,Reg0,k);
					      op sub(x,Direct tmpR,dest);
					      freeTmpReg tmpR
					  end
    | op sub _ = error "MipsCM.sub: mismatched args"


  fun notb(a,b) 	= op sub(a, Immed ~1, b)


 (* 
  * integer arithmetic with overflow trapping - addt subt mult divt
  *)
  fun addt (Immed ai,Immed bi,Direct rd) =
      (load_immed(ai,rd);  do_immed_arith(M.ADD,rd,rd,bi))
    | addt args = add args

  val subt = op sub


 (* The Mips multiplies two 32-bit quantities to get a 64-bit result.
  * That result fits in 32 bits if and only if the high-order word is zero
  * or negative one, and it has the same sign as the low order word.
  * Thus, we can add the sign bit of the low order word to the high order 
  * word, and we have overflow if and only if the result is nonzero.
  *)
  fun mult(ea,y as Direct y') =
      let val tmpR = getTmpReg()
      in 
	  (case ea
	   of Immed xi  => 
	       (do_immed_arith(M.ADD,tmpR,Reg0, xi);  mult(Direct tmpR,y))
	    | Direct x' => 
	       let val ok = C.newLabel()
	       in  emit (M.MULT(x',y'));
		   emit (M.MFLO y');
		   emit (M.SLT(y',y',RegOp (Reg0)));
		   emit (M.MFHI tmpR);
		   emit (M.ADD(tmpR,y',RegOp tmpR));
		   emit (M.MFLO y');
		   emitBRANCH(true,tmpR,Reg0,ok);
		   emit (M.LUI(tmpR,Immed16Off 32767));
		   emit (M.ADD(tmpR,tmpR,RegOp tmpR));
		   C.define ok
	       end
	    | _ => error "MipsCM.mult");
	  freeTmpReg tmpR
      end
    | mult _ = error "MipsCM.mult: result not a register"


 (*
  * divt(a,b) means b <- b div a 
  *)
  fun divt(Direct x',Direct y') = 
      let val oklabel = C.newLabel()
      in 
	  (* emit (M.DIV(y',x')); *)
	  emitBRANCH(false,Reg0,x',oklabel);
	  emit(M.BREAK 7);
	  C.define oklabel;
	  emit (M.DIV(y',x'));
	  emit (M.MFLO y')
      end
    | divt(Immed xi, y) = 
      let val tmpR = getTmpReg()
      in
	  do_immed_arith(M.ADD,tmpR,Reg0,xi);
	  divt(Direct tmpR,y);
	  freeTmpReg tmpR
      end
    | divt _ = error "MipsCM.divt: mismatched args"


  fun ashr(shamt,Direct rt,Direct rd) =
      (case shamt
	   of Direct rs => emit(M.SRAV(rd,rt,rs))
	    | Immed n      => 
	       if n >= 32 orelse n < 0 then
		   error "MipsCM.ashr: Too large a shift distance"
	       else
		   emit(M.SRA(rd,rt,Int5 n))
	    | _ => error "MipsCM.ashr")
    | ashr(shamt,Immed n,dst) = let val tmpR = getTmpReg()
				in  
				    load_immed(n,tmpR);
				    ashr(shamt,Direct tmpR,dst);
				    freeTmpReg tmpR
				end
    | ashr _ = error "MipsCM.ashr: bad args"


  fun ashl(shamt,Direct rt,Direct rd) =
      (case shamt
	   of Direct rs => emit(M.SLLV(rd,rt,rs))
	 | Immed n      => 
	       if n >= 32 orelse n < 0 then
		   error "MipsCM.ashl: Too large a shift distance"
	       else
		   emit(M.SLL(rd,rt,Int5 n))
	 | _ => error "MipsCM.ashl")
    | ashl(shamt,Immed n,dst) = let val tmpR = getTmpReg()
				in  
				    load_immed(n,tmpR);
				    ashl(shamt,Direct tmpR,dst);
				    freeTmpReg tmpR
				end
    | ashl _ = error "MipsCM.ashl: bad args"



  datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

  fun ibranch(cond,Immed a,Immed b,ImmedLab lab) =
      if  (case cond of EQL => a=b  | NEQ => a<>b | LSS => a<b |
			LEQ => a<=b | GTR => a>b  | GEQ => a>=b)  
      then  emitBRANCH(true,Reg0,Reg0,lab)
      else  ()
    | ibranch(cond,Immed n,Direct t,label) =
      let val tmpR = getTmpReg()
      in  
	  load_immed(n,tmpR);
	  ibranch(cond,Direct tmpR,Direct t,label);
	  freeTmpReg tmpR
      end
    | ibranch(cond,Direct rs,Immed n,label) =
     (*
      * could do a better job of this case (ref.G.Kane, table C.2)
      *)
      let val tmpR = getTmpReg()
      in 
	  load_immed(n,tmpR); 
	  ibranch(cond,Direct rs,Direct tmpR,label);
	  freeTmpReg tmpR
      end
    | ibranch(cond,Direct rs,Direct rt,ImmedLab lab) = 
      (case cond
       of NEQ => emitBRANCH(false,rs,rt,lab)
	| EQL => emitBRANCH(true,rs,rt,lab)
	| cond => let val tmpR = getTmpReg()
		  in (case cond
		      of LEQ => 
			     (emit(M.SLT(tmpR,rt,RegOp rs)); 
			      emitBRANCH(true,tmpR,Reg0,lab))
		       | GEQ => 
			     (emit(M.SLT(tmpR,rs,RegOp rt));
			      emitBRANCH(true,tmpR,Reg0,lab))
		       | LSS => 
			     (emit(M.SLT(tmpR,rs,RegOp rt));
			      emitBRANCH(false,tmpR,Reg0,lab))
		       | GTR => 
			     (emit(M.SLT(tmpR,rt,RegOp rs));
			      emitBRANCH(false,tmpR,Reg0,lab))
		       | _ => error "");
		      freeTmpReg tmpR
		  end)
    | ibranch _ = error "MipsCM.ibranch: bad args"


 (*
  * bbs - branch on bit set.
  *)
  fun bbs(Immed k, Direct y, ImmedLab label) =
      let val tmpR = getTmpReg()
      in
	  do_immed_arith(M.AND,tmpR,y,Bits.lshift(1,k));
	  emitBRANCH(false,tmpR,Reg0,label);
	  freeTmpReg tmpR
      end
    | bbs _ = error "MipsCM.bbs: bad args"


 (* 
  * rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) 
  *)
  fun rangeChk(Immed a, Immed b, ImmedLab lab) =
      if a<0 orelse a>=b then emitBRANCH(true,Reg0,Reg0,lab) else ()
    | rangeChk(Immed a, b, ImmedLab lab) =
      if a<0 then emitBRANCH(true,Reg0, Reg0, lab)  
      else ibranch(GEQ,Immed a,b, ImmedLab lab)
    | rangeChk(Direct a, Direct b', ImmedLab lab) = let
	  val tmpR = getTmpReg()
      in
	  emit(M.SLTU(tmpR,a,RegOp b')); 
	  emitBRANCH(true,Reg0,tmpR,lab);
	  freeTmpReg tmpR
      end
    | rangeChk(Direct a, Immed n, ImmedLab lab) = let
	  val tmpR = getTmpReg()
      in
	  do_immed_arith(M.SLTU,tmpR,a,n); 
	  emitBRANCH(true,Reg0,tmpR,lab);
	  freeTmpReg tmpR
      end
    | rangeChk _ = error "MipsCM.rangeChk"


  fun floatreg (Direct fpr) = (case reg_rep fpr 
			        of Freg' _ => fpr 
				 | _ => error "MipsCM.floatreg: expected floatreg")
    | floatreg _ = error "MipsCM.floatreg: expected floatreg"
  local 
      val real_tag = System.Tags.desc_reald
      val lowOff = E.low_order_offset

      fun store_float(n',dst,offset) = 
	   case (reg_rep n', dst)
	    of (Freg' n, Direct dst') =>
		if n mod 2 <> 0 then
		    error "MipsCM.store_float: bad float reg"
		else 
		    (do_immed_mem (M.SWC1,Freg(n+1-lowOff),dst',offset+4);
		     do_immed_mem (M.SWC1,Freg(n+lowOff),dst',offset))
	     | _ => error "MipsCM.store_float: bad args"

      fun load_float(dest',src,offset) =
	  case reg_rep dest'
	    of Freg' dest =>
		if dest mod 2 <> 0 then error "MipsCM.load_float.1"
		else (case src
		       of Direct src' => 
			   (do_immed_mem(M.LWC1,Freg(dest+lowOff),src',offset);
			    do_immed_mem(M.LWC1,Freg(dest+1-lowOff),src',offset+4))
		       | ImmedLab lab => 
			     let val tmpR = getTmpReg()
			     in emitSDI(LOADF(Freg dest,lab,offset,tmpR));
				 freeTmpReg tmpR
			     end
		       | _ => error "MipsCM.load_float.3")
	     | _ => error "MipsCM.load_float.2"

  in
      fun storefloat(src,Direct dst) =
	  (case reg_rep dst 
	    of Reg' result =>
		(store_float(floatreg src,dataptr,4);
		 let val tmpR = getTmpReg()
		 in
		     emit (M.ADD(tmpR,Reg0,Immed16Op real_tag));
		     emit (M.SW(tmpR,dataptr',Immed16Off 0));
		     emit (M.ADD(Reg result,dataptr',Immed16Op 4));
		     emit (M.ADD(dataptr',dataptr',Immed16Op 12));
		    freeTmpReg tmpR
		end)
	     | _ => error "MipsCM.storefloat: bad args")
	| storefloat _ = error "MipsCM.storefloat: bad args.2"

      fun loadfloat(src, dst) = load_float(floatreg dst,src,0)
					  (* y <- mem[x+4*(z-1)] *)
      fun fetchindexd(Direct x, y, z) =
	  (case z 
	    of Immed i   => load_float(floatreg y, Direct x, 4*(i-1))
	     | Direct z' => let val tmpR = getTmpReg()
			    in
				emit (M.SLL(tmpR,z',Int5 2));
				emit (M.ADD(tmpR,x,RegOp tmpR));
				load_float(floatreg y, Direct tmpR, ~4);
				freeTmpReg tmpR
			    end
	     | _ => error "MipsCM.fetchindexd")
	| fetchindexd _ = error "MipsCM.fetchindexd"

					  (* mem[y+4*(z-1)] <- x *)
      fun storeindexd(x, Direct y, z) =
	  (case z 
	    of Immed i => store_float(floatreg x,Direct y, 4*(i-1))
	     | Direct z' => let val tmpR = getTmpReg()
			    in
				emit (M.SLL(tmpR,z',Int5 2));
				emit (M.ADD(tmpR,y,RegOp tmpR));
				store_float(floatreg x,Direct tmpR,~4);
				freeTmpReg tmpR
			    end
	     | _ => error "MipsCM.storeindexd")
	| storeindexd _ = error "MipsCM.storeindexd"
  end
  local 
      fun floating_arith f (x,y,z) = emit(f(floatreg x,floatreg y,floatreg z))

      fun compare(LSS,op1,op2) = (emit(M.SLT_DOUBLE(op1,op2)); true)
	| compare(GEQ,op1,op2) = (emit(M.SLT_DOUBLE(op1,op2)); false)
	| compare(EQL,op1,op2) = (emit(M.SEQ_DOUBLE(op1,op2)); true)
	| compare(NEQ,op1,op2) = (emit(M.SEQ_DOUBLE(op1,op2)); false)
	| compare(LEQ,op1,op2) = compare(GEQ,op2,op1)
	| compare(GTR,op1,op2) = compare(LSS,op2,op1)
  in
      fun fmuld(x,y,z) 	  = floating_arith M.MUL_DOUBLE (z,x,y)
      fun fdivd(x,y,z) 	  = floating_arith M.DIV_DOUBLE (z,x,y)
      fun faddd(x,y,z) 	  = floating_arith M.ADD_DOUBLE (z,x,y)
      fun fsubd(x,y,z) 	  = floating_arith M.SUB_DOUBLE (z,x,y)
      fun fnegd(op1,result) = emit(M.NEG_DOUBLE(floatreg result,floatreg op1))
      fun fabsd(op1,result) = emit(M.ABS_DOUBLE(floatreg result,floatreg op1))

      fun fbranchd (cond, op1, op2, ImmedLab label) = 
	  emitBRANCH_COP1(compare(cond,floatreg op1,floatreg op2),label)
	| fbranchd _ = error "MipsCM.fbranchd: insane target"
  end

  fun cvti2d(Direct src,dst as Direct dst') = 
      (case (reg_rep src, reg_rep dst')
        of (Reg' _, Freg' _) => (emit (M.MTC1(src, dst'));
				   emit (M.CVTI2D(dst', dst'))))
    | cvti2d(Immed n, dst) = 
                   let val tmpR = getTmpReg()
		    in  do_immed_arith(M.ADD,tmpR,Reg0,n);
			cvti2d(Direct tmpR,dst);
			freeTmpReg tmpR
		   end

  val comment = C.comment
end






