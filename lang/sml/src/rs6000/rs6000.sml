(* Copyright (c) 1992 by AT&T Bell Laboratories *)

(* IBM RS6000 Cmachine implementation *)

functor RS6000CM (structure C : CODER
 	  	  sharing type C.instruction = RS6000InstrSet.instruction
		      and type C.sdi = RS6000InstrSet.sdi) : CMACHINE =
struct

  structure M = RS6000InstrSet
  open M

  val error 		= fn msg => ErrorMsg.impossible ("RS6kCM." ^ msg)

  type EA		= C.label M.EA
  exception BadReal 	= C.BadReal
  val align		= fn () => ()
  val mark 		= C.mark
  val emitlong 		= C.emitLong
  val realconst 	= C.emitReal
  val emitstring 	= C.emitString
  val newlabel 		= M.ImmedLab o C.newLabel
  val immed       	= M.Immed
  val emitSDI		= C.emitSDI
  val emit		= C.emit

  fun emitlab(k,ImmedLab lab) = C.emitLabel(lab,k)
    | emitlab _ = error "emitlab"

  fun define(ImmedLab lab) = C.define lab
    | define _ = error "RS6kCM.define"

  (** 
     Register Map
     Reg   gc   desc
     -------------------------------------
     0	   n   odd ball register
     1	   n   stack pointer	(not used in ML)
     2     n   TOC 		(not used in ML)
     3-13  y   miscregs
     14	   y   data pointer
     15	   n   heap limit 
     16	   y   store pointer
     17    y   standardlink
     18    y   standardclosure
     19    y   standardarg
     20    y   standardcont
     21    y   exception pointer
     22    y   varptr
     23    y   base pointer
     24-27 y   misc regs
     28    n   temporary (also gclink)
     29-31 n   temporaries
  **)

  val varptr_indexable	 	   = true
  val stackptr as Direct stackptr' = Direct(M.stackReg)

  val dataptr  as Direct dataptr'  = Direct(M.allocReg)
  val limitptr as Direct limitptr' = Direct(M.limitReg)
  val storeptr as Direct storeptr' = Direct(Reg 16)
  val standardlink		   = Direct(Reg 17)
  val standardclosure		   = Direct(Reg 18)
  val standardarg		   = Direct(Reg 19)
  val standardcont		   = Direct(Reg 20)
  val exnptr	  		   = Direct(M.exnptrReg)
  val varptr 			   = Direct(Reg 22)
  val miscregs 			   = map (Direct o Reg) 
      					 [24,25,26,27,3,4,5,6,7,8,9,10,11,12,13]
  val gcLinkReg			   = Reg 28

  val floatregs: EA list	   = map (Direct o Freg)
      					 [1,2,3,4,5,6,7,8,9,10,11,12,13]
  val savedfpregs: EA list	   = map (Direct o Freg) 
					 [14,15,16,17,18,19,20,21,22,23,
					  24,25,26,27,28,29,30,31]
  val arithtemps: EA list	   = []
  val tmpFreg			   = Freg 0

  local
      exception NoTmpRegs
      val front		= ref 0
      val back		= ref 0
      val tmpRegs	= [M.maskReg,Reg 30,Reg 31]
      val qsize		= length tmpRegs + 1
      val queue 	= Array.array(qsize,~1)
      fun insert(Reg r) = Array.update(queue,!back,r) 
				  before back := (!back+1) mod qsize
	| insert _      = error "insert"
      fun remove() 	= if !front = !back then raise NoTmpRegs
			  else Array.sub(queue,!front) 
				   before front := (!front+1) mod qsize
      val _ = app insert tmpRegs
  in  
      fun getTmpReg()    = Reg(remove())
      fun freeTmpReg reg = insert reg			        
  end

  fun emitBRANCH(cond,bool,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in 
	  emitSDI(M.BRANCH(cond,bool,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end

  fun emitFBRANCH(cond,cr,bool,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in
	  emitSDI(M.FBRANCH(cond,cr,bool,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end

  datatype immedSize = IMMED16 | IMMED32

  fun immed_size n = if (~32768 <= n) andalso (n < 32768) then IMMED16
		     else IMMED32

  fun do_immed_signed(instr,rt,ra,si) = 
      case (immed_size si) 
	of IMMED16 => emit (instr(rt,ra,Immed16Op si))
         | IMMED32 => let
	       val (hi,lo) = M.split si
	       val tmpR = getTmpReg()
	   in
	       emit (M.LIU(tmpR, Immed16Op hi));
	       emit (M.A(tmpR,tmpR,Immed16Op lo));
	       emit (instr(rt,ra,RegOp tmpR));
	       freeTmpReg tmpR
           end

  fun load_immed(rt,n) = 
      case (immed_size n) 
        of IMMED16 => emit (M.CAL(rt,Reg 0,Immed16Op n))
         | IMMED32 => let
	       val (hi,lo) = M.split n
           in
	       emit (M.LIU(rt,Immed16Op hi));
	       emit (M.A(rt,rt,Immed16Op lo))
           end
	       
 (* move(a,b) means a -> b *)
  fun move (Direct(fp1 as Freg _),Direct(fp2 as Freg _)) = emit (M.FMR(fp2,fp1))
    | move (_, Direct(Freg _))        = error "move: bad src"
    | move (Immed n, Direct dst)      = load_immed(dst,n)
    | move (ImmedLab lab, Direct dst) = emitSDI(LOADADDR(dst,lab,0))
    | move (Direct src, Direct dst)   = if src = dst 
				        then ()
				        else emit (M.AND(dst,src,RegOp src))
    | move _ 			      = error "move"

  fun compare_immed(ra,n) = 
      if   n >= ~32768 andalso n <= 32767 
      then emit (M.CMP(ra,Immed16Op n))
      else let val tmpR = getTmpReg()
	   in 
	       move(Immed n,Direct tmpR);
	       emit (M.CMP(ra,RegOp tmpR));
	       freeTmpReg tmpR
	   end

  fun jmp (Direct r)     = (emit (M.MTSPR(M.LR,r)); emit (M.BR()))
    | jmp (ImmedLab lab) = emit (B(Label24Off(M.POSLAB lab,0)))
    | jmp _		 = error "jmp"


  val startgc_offset = 4

  fun testLimit() = emit (M.CMPL(limitptr',dataptr'))

  fun beginStdFn (ImmedLab lab,Direct reg) = emitSDI(M.SETBASEADDR(lab,reg))
    | beginStdFn _  			   = error "beginStdFn"

  fun checkLimit(max_allocation, restart, mask) = let
        val lab = C.newLabel()
	val tmpR = getTmpReg()
     in 
	 if max_allocation > 4096 
         then (do_immed_signed(M.A,tmpR,dataptr',max_allocation-4096);
	       emit (M.CMPL(limitptr',tmpR)))
	 else ();
	 emitBRANCH(M.GT,true,lab);
	 emit (M.L(tmpR,stackptr',Immed16Op startgc_offset));
	 emit (M.MTSPR(M.LR,tmpR));
	 freeTmpReg tmpR;
	 move(mask, Direct M.maskReg);
	 move(restart, Direct gcLinkReg);
	 emit (M.BR());
	 C.define lab
     end	 

 (* jmpindexb(x,y) means pc <- x + y *)
  fun jmpindexb (ImmedLab lab,Direct y) = let
        val tmpR = getTmpReg()
      in
	  emitSDI(M.LOADADDR(tmpR,lab,0));
	  emit (M.A(tmpR,y,RegOp tmpR));
	  emit (M.MTSPR(M.LR,tmpR));
	  freeTmpReg tmpR;
	  emit (M.BR())
      end
    | jmpindexb _ = error "jmpindexb"

  fun record(vl, Direct z) = let
        open CPS
	val len = List.length vl
	fun f(_,i,nil) = ()
	  | f((t1,t2),i,(Direct r, SELp(j,p))::rest) = 
	       (** follow ptrs to get the item  **)
	        (do_immed_signed(M.L,t1,r,j*4);
		 f((t2,t1),i,(Direct t1,p)::rest))
	  | f(t,i,(Direct r,OFFp 0)::rest) = 
	       (**  simple store, last first  **) 
	        (do_immed_signed(M.ST,r,dataptr',i*4);
		 f(t,i-1,rest))
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
		(emit (M.A(t1,r,Immed16Op(4*j))); 
		 f((t2,t1),i,(Direct t1,OFFp 0)::rest))
	  | f((t1,t2),i,(ea,p)::rest) =
	       (* convert to register-based  *)
		(move(ea,Direct t1);  
		 f((t2,t1),i,(Direct t1,p)::rest))
	val tmpR1 = getTmpReg()
	val tmpR2 = getTmpReg()
      in 
       (* store first word in 0(dataptr') *)
	f((tmpR1,tmpR2),len-1,rev vl); 
	freeTmpReg tmpR1;
	freeTmpReg tmpR2;
	emit (M.A(z,dataptr',Immed16Op 4));
	do_immed_signed(M.A,dataptr',dataptr',4*len)
      end
    | record _ = error "record"

    fun recordStore (x, y, _) = 
	record ([(Immed(System.Tags.make_desc(3, System.Tags.tag_record)), 
		  CPS.OFFp 0),(x, CPS.OFFp 0), (y, CPS.OFFp 0), 
		 (storeptr, CPS.OFFp 0)], 
		storeptr)

  fun select (i,Direct v',Direct w)    = do_immed_signed(M.L,w,v',i*4)
    | select (i,ImmedLab lab,Direct w) = emitSDI(LOAD(w,lab,i*4))
    | select _ 			       = error "select"


  fun offset (i,Direct v',Direct w)    = do_immed_signed(M.A,w,v',i*4)
    | offset (i,ImmedLab lab,Direct w) = let val tmpR = getTmpReg()
					 in
					     emitSDI(LOADADDR(tmpR,lab,0));
					     do_immed_signed(M.A,w,tmpR,i*4);
					     freeTmpReg tmpR
					 end
    | offset _ 			       = error "offset"


  fun fetchindexb(Direct x,Direct y,Immed indx) = do_immed_signed(M.LBZ,y,x,indx)
    | fetchindexb(Direct x,Direct y,Direct indx)= emit (M.LBZ(y,x,RegOp indx))
    | fetchindexb _ 				= error "fetchindexb"


  fun storeindexb(Immed xi,y,z) = let 
         val tmpR = getTmpReg()
      in
	  load_immed(tmpR,xi);
	  storeindexb(Direct tmpR,y,z);
	  freeTmpReg tmpR
      end
    | storeindexb(Direct x,Direct y,Direct indx)= emit (M.STB(x,y,RegOp indx))
    | storeindexb(Direct x,Direct y,Immed indx) = do_immed_signed(M.STB,x,y,indx)
    | storeindexb _ 				= error "storeindexb"

  fun fetchindexl(x,Direct y,Direct z') = let
        val tmpR = getTmpReg()
      in
	  emit (M.SL(tmpR,z',M.Int5Shift 1));
	  (case x 
	     of Direct x'    => ( emit (M.A(tmpR,x',RegOp tmpR));
			          emit (M.L(y,tmpR,Immed16Op ~2)))
	      | Immed n      => do_immed_signed(M.L,y,tmpR,n-2)
	      | ImmedLab lab => 
		   let val tmpR2 = getTmpReg()
		   in
		       emitSDI(M.LOADADDR(tmpR2,lab,0));
		       emit (M.A(tmpR,tmpR,RegOp tmpR2));
		       freeTmpReg tmpR2;
		       emit (M.L(y,tmpR,Immed16Op ~2))
		   end);
	  freeTmpReg tmpR
      end
    | fetchindexl(x,Direct y,Immed z') =  
      (case x
	 of Direct x'    => do_immed_signed(M.L,y,x',2*(z'-1))
	  | Immed n      => do_immed_signed(M.L,y,Reg 0,n+2*(z'-1))
	  | ImmedLab lab => emitSDI(LOAD(y,lab,2*(z'-1))))
    | fetchindexl _ = error "fetchindexl"

  fun storeindexl(Direct x,Direct y,Direct z) = let
        val tmpR = getTmpReg()
      in 
        emit (M.SL(tmpR,z,Int5Shift 1));
	emit (M.A(tmpR,tmpR,RegOp y));
	emit (M.ST(x,tmpR,Immed16Op ~2));
	freeTmpReg tmpR
      end
    | storeindexl(Direct x,Direct y,Immed zi) = 
        do_immed_signed(M.ST,x,y,2*(zi-1))
    | storeindexl(Immed xi,y,z) =  let val tmpR = getTmpReg()
				   in
				       move(Immed xi,Direct tmpR);
				       storeindexl(Direct tmpR,y,z);
				       freeTmpReg tmpR
				   end
    | storeindexl(ImmedLab lab,y,z) = let val tmpR = getTmpReg()
				      in
					  emitSDI(M.LOADADDR(tmpR,lab,0));
					  storeindexl(Direct tmpR,y,z);
					  freeTmpReg tmpR
				      end
    | storeindexl(Direct x,ImmedLab lab,Immed zi) = let
        val tmpR = getTmpReg()
      in
	  emitSDI(M.LOADADDR(tmpR,lab,0));  
	  do_immed_signed(M.ST,x,tmpR,2*(zi-1));
	  freeTmpReg tmpR
      end
    | storeindexl _ = error "MipsCM.storeindexl: bad args"

  local
    fun three f (Direct x',Direct y',Immed zi)     = do_immed_signed(f,x',y',zi)
      | three f (Direct x',Direct y',Direct z')    = emit (f(x',y',RegOp z'))
      | three f (Direct x',Direct y',ImmedLab lab) = let
	  val tmpR = getTmpReg()
	in
	    emitSDI(M.LOADADDR(tmpR,lab,0));  
	    emit (f(x',y',RegOp tmpR));
	    freeTmpReg tmpR
	end
      | three f (Direct x,ea,Direct z) = three f (Direct x,Direct z,ea)
      | three _ _ 		       = error "MipsCM.three: bad args"
  in
    fun add(x,y,z) 		= three M.A   (z,x,y)
    fun orb(x,y,z) 		= three M.OR  (z,x,y) 
    fun andb(x,y,z)		= three M.AND (z,x,y)
    fun xorb(x,y,z)		= three M.XOR (z,x,y)
  end

  fun trapOnOverflow () = let val lab = C.newLabel()
			  in 
			      emitBRANCH(M.SO,false,lab);
			      emit(M.TRAP());
			      C.define lab
			  end
  fun trapOnDivZero () = let val lab = C.newLabel()
			 in 
			     emitBRANCH(M.SO,false,lab);
			     emit(MTFSB1 5);
			     emit(M.TRAP());
			     C.define lab
			 end
  local 
    fun move2reg (Direct(Reg r)) = (Reg r,NONE)
      | move2reg (Immed n)       = let val tmpR = getTmpReg()
				   in 
				       move(Immed n, Direct tmpR);
				       (tmpR,SOME tmpR)
				   end
      | move2reg (ImmedLab lab)  = let val tmpR = getTmpReg()
				   in 
				       move(ImmedLab lab, Direct tmpR);
				       (tmpR, SOME tmpR)
				   end
      | move2reg _ 		 = error "move2reg"

    fun free NONE = () 
      | free (SOME r) = freeTmpReg r
  in
    fun addt(x,y,Direct z) = let val (x',tmpx) = move2reg x
				 val (y',tmpy) = move2reg y
			     in 
				 emit (M.AO(z,x',y'));
				 trapOnOverflow();
				 free tmpx; 
				 free tmpy
			     end
      | addt _ 		   = error "addt"
				      
    fun mult(x,Direct y) = let val (x',tmpx) = move2reg x
			   in
			       emit (MULSO(y,x',y));
			       trapOnOverflow();
			       free tmpx
			   end
      | mult _ 		 = error "mult"
  end

  fun sub (Direct x,Direct y,Direct z) = emit (M.SF(z,x,RegOp y))
    | sub (Direct x,Immed yi,Direct z) = do_immed_signed(M.SF,z,x,yi)
    | sub (Immed xi,y,z)               = let  val tmpR = getTmpReg()
					 in
					     move(Immed xi,Direct tmpR);
					     sub(Direct tmpR,y,z);
					     freeTmpReg tmpR
					 end
    | sub _			       = error "sub"

  fun notb(a,b)	= sub(a, Immed ~1, b)

  local 
      fun subtract(Direct x,Direct y,Direct z) = emit (SFO(z,x,y))
	| subtract(Immed xi,y,z)  = let val tmpR = getTmpReg()
				    in
					move(Immed xi,Direct tmpR);
					subtract(Direct tmpR,y,z);
					freeTmpReg tmpR
				    end
	| subtract(x,Immed yi,z)  = let val tmpR = getTmpReg()
				    in 
					move(Immed yi,Direct tmpR);
					subtract(x,Direct tmpR,z);
					freeTmpReg tmpR
				    end
	| subtract _ 	          = error "subtract"
  in
      fun subt arg = (subtract arg; trapOnOverflow())
  end

			(* divt(a,b) means b <- b / a *)
  local
    fun divide (Direct x,Direct y) = emit (M.DIVS(y,y,x))
      | divide (Immed xi,Direct y) = let val tmpR = getTmpReg()
				     in 
					 move(Immed xi,Direct tmpR);
					 emit (M.DIVS(y,y,tmpR));
					 freeTmpReg tmpR
				     end
      | divide _		   = error "divide"
  in
      fun divt arg = (divide arg; trapOnDivZero())
  end
				      
  fun ashl (Direct rs,Direct rt,Direct rd) = emit (M.SL(rd,rt,RegShift rs))
    | ashl (Immed n,Direct rt,Direct rd) = 
      if n >= 32 orelse n < 0 then
	  error "ashl: shift distance"
      else
	  emit (M.SL(rd,rt,Int5Shift n))
    | ashl(shamt,Immed n,dst) = let 
        val tmpR = getTmpReg()
      in  
	  move(Immed n, Direct tmpR);
	  ashl(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end
    | ashl _ = error "ashl"

  fun ashr (Direct rs,Direct rt,Direct rd) = 
      emit (M.SRA(rd,rt,RegShift rs))
    | ashr (Immed n,Direct rt,Direct rd) = 
      if n >= 32 orelse n < 0 then
	  error "ashr: shift distance"
      else
	  emit (M.SRA(rd,rt,Int5Shift n))
    | ashr(shamt,Immed n,dst) = let
        val tmpR = getTmpReg()
      in  
	  move(Immed n,Direct tmpR);
	  ashr(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end
    | ashr _ = error "MipsCM.ashr: bad args"

  local 
    fun floatreg (Direct(fpr as Freg _)) = fpr
      | floatreg _ 			 = error "floatreg"

    fun floating_arith f (x,y,z) = let 
          val lab = C.newLabel()
	in
	    emit (f(floatreg x,floatreg y,floatreg z));
	    emitFBRANCH(M.FEX,1,false,lab);
	    emit(M.TRAP());
	    C.define lab
        end

    val real_tag = System.Tags.desc_reald

    fun store_float(Freg fp,Direct dst,offset) = let
	  val tmpR = getTmpReg()
        in
	    emit(M.STFD(Freg fp,stackptr',Immed16Op M.fLoadStoreOff));
	    emit(M.L(tmpR,stackptr',Immed16Op M.fLoadStoreOff));
	    do_immed_signed(M.ST,tmpR,dst,offset);
	    emit(M.L(tmpR,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	    do_immed_signed(M.ST,tmpR,dst,offset+4);
	    freeTmpReg tmpR
	end
      | store_float _ = error "store_float"	    

    fun load_float (Freg dst,Direct src,offset) = let
	  val tmpR = getTmpReg()
        in
	    do_immed_signed(M.L,tmpR,src,offset);
	    emit(M.ST(tmpR,stackptr',Immed16Op M.fLoadStoreOff));
	    do_immed_signed(M.L,tmpR,src,offset+4);
	    emit(M.ST(tmpR,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	    emit(M.LFD(Freg dst,stackptr',Immed16Op M.fLoadStoreOff));
	    freeTmpReg tmpR
	    
        end
      | load_float (Freg dst,ImmedLab lab,offset) = let
 	  val tmpR = getTmpReg()
	in
	    emitSDI(LOADF(Freg dst,lab,offset,tmpR));
	    freeTmpReg tmpR
	end
      | load_float _ = error "load_float"
  in
      fun fmuld(x,y,z) 	    = floating_arith M.FMO (z,x,y)
      fun fdivd(x,y,z) 	    = floating_arith M.FDO (z,x,y)
      fun faddd(x,y,z) 	    = floating_arith M.FAO (z,x,y)
      fun fsubd(x,y,z) 	    = floating_arith M.FSO (z,x,y)
      fun fnegd(op1,result) = emit (M.FNEG(floatreg result,floatreg op1))
      fun fabsd(op1,result) = emit (M.FABS(floatreg result,floatreg op1))

      fun storefloat(src,Direct(Reg result)) =
	    (store_float(floatreg src,dataptr,4);
	     let val tmpR = getTmpReg()
	     in
		 load_immed(tmpR,real_tag);
		 emit (M.ST(tmpR,dataptr',Immed16Op 0));
		 emit (M.A(Reg result,dataptr',Immed16Op 4));
		 emit (M.A(dataptr',dataptr',Immed16Op 12));
		 freeTmpReg tmpR
	     end)
	| storefloat  _ = error "storefloat"
  
      fun loadfloat(src, dst) = load_float(floatreg dst,src,0)

     (* fetchindexd (x,y,z) y <- mem[x+4*(z-1)] *)
      fun fetchindexd (Direct x,y,Immed i) = 
	    load_float(floatreg y, Direct x, 4*(i-1))
	| fetchindexd (Direct x,y,Direct z) = let
	    val tmpR = getTmpReg()
  	  in
	    emit (M.SL(tmpR,z,Int5Shift 2));
	    emit (M.A(tmpR,x,RegOp tmpR));
	    load_float(floatreg y,Direct tmpR,~4);
	    freeTmpReg tmpR
	  end
	| fetchindexd _ = error "fetchindexd"

    fun storeindexd (x,Direct y,Immed i) = 
	  store_float(floatreg x,Direct y, 4*(i-1))
      | storeindexd (x,Direct y,Direct z) = let
          val tmpR = getTmpReg()
	in
	    emit (M.SL(tmpR,z,Int5Shift 2));
	    emit (M.A(tmpR,y,RegOp tmpR));
	    store_float(floatreg x,Direct tmpR,~4);
	    freeTmpReg tmpR
    	end
      | storeindexd _ = error "storeindexd"		
  end

  datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

  local
    fun compare(ra,Immed si) = compare_immed(ra,si)
      | compare (ra,Direct rb) = emit (M.CMP(ra,RegOp rb))
      | compare _ = error "compare"

    fun branch(cond,lab) = 	   
	case cond
	  of NEQ => emitBRANCH(M.EQ,false,lab)
	   | EQL => emitBRANCH(M.EQ,true,lab)
	   | GTR => emitBRANCH(M.GT,true,lab)
	   | LEQ => emitBRANCH(M.GT,false,lab)
	   | LSS => emitBRANCH(M.LT,true,lab)
	   | GEQ => emitBRANCH(M.LT,false,lab)
  in
    fun ibranch(cond,Immed a,Immed b,ImmedLab lab) =
	if  (case cond of EQL => a=b  | NEQ => a<>b | LSS => a<b |
			  LEQ => a<=b | GTR => a>b  | GEQ => a>=b)  
	then emit (M.B(Label24Off(POSLAB lab,0)))
	else  ()
      | ibranch(cond,Direct rs,Immed n,ImmedLab lab) = 
	  (compare_immed(rs,n); branch(cond,lab))
      | ibranch(cond,Immed n,rb,lab) = let
	  val tmpR as Direct tmpR' = Direct(getTmpReg())
        in
	   move(Immed n,tmpR);
	   ibranch(cond,tmpR,rb,lab);
	   freeTmpReg tmpR'
        end
      | ibranch(cond,Direct ra,Direct rb,ImmedLab lab) = 
	  (compare(ra,Direct rb); branch(cond,lab))
      | ibranch _ = error "ibranch"
  end

  fun fbranchd(cond,Direct fra,Direct frb,ImmedLab lab) = 
        (emit (M.FCMP(fra,frb));
	 case cond 
	   of NEQ => emitFBRANCH(M.FE,2,false,lab)
	    | EQL => emitFBRANCH(M.FE,2,true,lab)
	    | GEQ => emitFBRANCH(M.FL,2,false,lab)
	    | LSS => emitFBRANCH(M.FL,2,true,lab)
	    | GTR => emitFBRANCH(M.FG,2,true,lab)
	    | LEQ => emitFBRANCH(M.FG,2,false,lab))
    | fbranchd _ = error "fbranch"

 (* 
  * rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) 
  *)
  fun rangeChk(Immed a,Immed b,ImmedLab lab) =
        if a<0 orelse a>=b then emit (M.B (Label24Off(POSLAB lab,0))) else ()
    | rangeChk(Immed a, b, ImmedLab lab) =
        if a<0 then emit (M.B (Label24Off(POSLAB lab,0)))
        else ibranch(GEQ,Immed a,b, ImmedLab lab)
    | rangeChk(Direct a, Direct b, ImmedLab lab) = let
        val tmpR = getTmpReg()
      in
	  emit (M.CMPL(a,b));
	  emitBRANCH(M.LT,false,lab);
	  freeTmpReg tmpR
      end
    | rangeChk(a,Immed n,lab) = let
        val tmpR = getTmpReg()
      in
	  move(Immed n, Direct tmpR);
	  rangeChk(a,Direct tmpR,lab);
	  freeTmpReg tmpR
      end
    | rangeChk _ = error "rangeChk"

 (* Should implement ANDcc and do this better *)
  fun bbs(Immed k,Direct y,ImmedLab label) =
      let val tmpR = getTmpReg()
      in
	  do_immed_signed(M.AND,tmpR,y,Bits.lshift(1,k));
	  compare_immed(tmpR,Bits.lshift(1,k));
	  freeTmpReg tmpR;
	  emitBRANCH(M.EQ,true,label)
      end
    | bbs _ = error "MipsCM.bbs: bad args"


  val cvti2dTmpOffset   = 16
  val cvti2dConstOffset = 8
  fun cvti2d(Direct(src as Reg _),Direct(dst as Freg _)) = let
      val tmpR = getTmpReg()
    in
	emit(M.XORU(tmpR,src,Immed16Op 32768));
	emit(M.ST(tmpR,stackptr',Immed16Op(cvti2dTmpOffset+4)));
	emit(M.LIU(tmpR,Immed16Op 17200));
	emit(M.ST(tmpR,stackptr',Immed16Op cvti2dTmpOffset));
	emit(M.LFD(dst,stackptr',Immed16Op cvti2dTmpOffset));
        emit(M.LFD(tmpFreg,stackptr',Immed16Op cvti2dConstOffset));
	emit(M.FSO(dst,dst,tmpFreg));
	freeTmpReg tmpR
    end

  val comment = C.comment
end
