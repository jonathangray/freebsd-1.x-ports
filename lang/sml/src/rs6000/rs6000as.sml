(** IBM rs6000 assembly code generator **)

structure RS6000AsmStream = 
    struct
	val asmStream = ref std_out
    end

structure RS6000AssemblyEmitter : EMITTER = 
struct
  structure M = RS6000InstrSet
  open M

  fun error msg = ErrorMsg.impossible ("RS6000AsCode." ^ msg)

  val loc = ref 0
  fun advance n = loc := !loc + n

  local
      val hexDigits = "0123456789abcdef"
      fun f (0, l) = l
	| f (n, l) =
	  (f(Bits.rshift(n,4),chr(ordof(hexDigits,Bits.andb(n,15)))::l))
      fun cvt 0 = ["0","x","0"]
	| cvt n = ("0"::"x"::f(n, nil))
  in
      fun itoa i = implode(if (i < 0) then "-" :: cvt(~i) else cvt i)
  end

  fun emit s = output(!RS6000AsmStream.asmStream,s)

  fun newLine() = emit "\n"
  fun emitLong i = (emit ".long\t"; emit(itoa i); newLine(); advance 4)
  local
      fun oct i = let val m = Integer.makestring
		  in m(i quot 64)^m((i quot 8)mod 8)^m(i mod 8)
		  end
      fun c_char "\n" = "\\n"
	| c_char "\t" = "\\t"
	| c_char "\\" = "\\\\"
	| c_char "\"" = "\\\""
	| c_char c = if ord c < 32 then "\\"^oct(ord c) else c
      fun a_str s = implode(map c_char (explode s))
  in
      fun emitString s = 
	  (emit ".ascii \""; emit(a_str s); emit "\"\n"; 
	   emit ".align\t4\n"; advance(size s))
  end 

  fun emitReal r = (emit ".double\t"; emit r; newLine(); advance 8)

  fun emitLabel (INFO{nameOf,...}) lab = emit(nameOf lab)
  fun emitAddr (info as INFO{addrOf,...}) (lab,k) = 
      (emit "\t.long\t"; emitLabel info lab; emit "\t## "; 
       emit(itoa(addrOf lab - !loc)); newLine(); advance 4)

  fun define (info as INFO{addrOf,...}) lab = 
      (emitLabel info lab; emit ":\t## "; emit(itoa (addrOf lab)); newLine())

  local open System.Tags
  in 
      fun mark() = (emit "\t.long\tMAKE_DESC(((.-L0)/4+1),tag_backptr)\t## ";
		    emit (itoa (make_desc((!loc+4)quot 4,tag_backptr))); 
		    newLine(); advance 4)
  end 

  fun emitInstr info I = 
    let fun comma() = emit ", "

	fun emitReg reg =
	    emit (case reg 
		    of Reg r =>  "r" ^ Integer.makestring r
		     | Freg fp => "f" ^ Integer.makestring fp)

	fun emitSpecial M.LR = emit "lr"
	fun emit_eaOpnd (Immed16Op n) = emit(makestring n)
	  | emit_eaOpnd (RegOp r) = emitReg r
	  | emit_eaOpnd (LabelOp lab) = emit(makestring(M.labelValue info lab))
	  | emit_eaOpnd (HiLabOp lab) = emit(makestring(M.hiLabelValue info lab))
	  | emit_eaOpnd (LoLabOp lab) = emit(makestring(M.loLabelValue info lab))

	fun emit_3regs(rs,ra,rb) = (emitReg rs; comma();
				    emitReg ra; comma();
				    emitReg rb; comma())

	fun emit_2reg_ea(rs,ra,ea) = (emitReg rs; comma();
				      emitReg ra; comma();
				      emit_eaOpnd ea)

	fun emit_shift(ra,rb,M.RegShift rs) = emit_3regs(ra,rb,rs)
	  | emit_shift(ra,rb,M.Int5Shift n) = (emitReg ra; comma();
					       emitReg rb; comma();
					       emit(makestring n))

	fun emit_branch24Opnd opnd = let
	      val labOff = M.labBranch24Off info opnd
	    in
		emit(makestring(labOff - (!loc div 4)))
	    end

	fun emit_branch16Opnd opnd = let
	      val labOff = M.labBranch16Off info opnd
	    in
		emit(makestring(labOff - (!loc div 4)))
	    end

	fun emit_cc cc = 
	    emit ("cr0." ^ (case cc of M.LT => "lt"
	                             | M.GT => "gt"
				     | M.EQ => "eq"
				     | M.SO => "so"
				     | _    => error "emit_cc"))

	fun emit_fcc fcc = 
	    emit ("cr2." ^ (case fcc of M.FL  => "lt"
		 	              | M.FG  => "gt"
				      | M.FE  => "eq"
		 	 	      | M.FO  => "so"
			              | M.FX  => "fx"
				      | M.FEX => "fex"
				      | M.VX  => "vx"
				      | M.OX  => "ox"
				      | _     => error "emit_fcc"))
    in (emit "\t";
	(case I 
	 of M.NOP 		   => error "emitInstr: M.NOP"
          | M.B arg		   => (emit "b\t"; emit_branch24Opnd arg)
	  | BB (cc,cond,lab)	   => (emit(if cond then "bbt\t" else "bbf\t");
				       emit_cc cc; comma(); 
				       emit_branch16Opnd lab)
	  | BBF	(fcc,cr,cond,lab)  => (emit(if cond then "bbt\t" else "bbf\t");
				       emit("cr" ^ makestring cr); comma();
				       emit_fcc fcc; comma(); 
				       emit_branch16Opnd lab)
	  | BR ()		   => emit "br\t"
	  | LBZ(a as(_,_,RegOp _)) => (emit "lbzx\t"; emit_2reg_ea a)
          | LBZ arg		   => (emit "lbz\t"; emit_2reg_ea arg)
	  | L(a as(_,_,RegOp _))   => (emit "lx\t"; emit_2reg_ea a)
          | L arg		   => (emit "l\t"; emit_2reg_ea arg)
	  | LFD	arg		   => (emit "lfd\t"; emit_2reg_ea arg)
          | LIU(r,ea)		   => (emit "liu\t"; emitReg r; comma(); 
				       emit_eaOpnd ea)
          | MTSPR(spr,r)	   => (emit "mtspr\t"; emitSpecial spr; 
				       comma(); emitReg r)
	  | CAL(a as(_,_,RegOp _)) => (emit "cax\t"; emit_2reg_ea a)
	  | CAL arg		   => (emit "cal\t"; emit_2reg_ea arg)
	  | STB(a as(_,_,RegOp _)) => (emit "stbx\t"; emit_2reg_ea a)
	  | STB arg		   => (emit "stb\t"; emit_2reg_ea arg)
	  | ST(a as(_,_,RegOp _))  => (emit "stx\t"; emit_2reg_ea a) 
	  | ST arg		   => (emit "st\t"; emit_2reg_ea arg)
          | STFD arg		   => (emit "stfd\t"; emit_2reg_ea arg)

	  | A arg		   => (emit "a\t"; emit_2reg_ea arg)
          | AO arg		   => (emit "ao.\t"; emit_3regs arg)
          | FAO	arg		   => (emit "fa.\t"; emit_3regs arg)

	  | SF arg		   => (emit "sf\t"; emit_2reg_ea arg)
	  | SFO	arg		   => (emit "sfo.\t"; emit_3regs arg)
          | FSO	arg		   => (emit "fs.\t"; emit_3regs arg)

          | MULSO arg		   => (emit "mulso.\t"; emit_3regs arg)
          | FMO arg		   => (emit "fm.\t"; emit_3regs arg)

          | DIVS arg		   => (emit "divso.\t"; emit_3regs arg)
          | FDO	arg		   => (emit "fd.\t"; emit_3regs arg)

          | FNEG(fp1,fp2)	   => (emit "fneg\t"; emitReg fp1; comma();
				       emitReg fp2)
	  | FABS(fp1,fp2) 	   => (emit "fabs\t"; emitReg fp1; comma();
				       emitReg fp2)
	  | CMP	(r,ea)		   => (emit "cmp\t"; emitReg r; comma();
				       emit_eaOpnd ea)
	  | CMPL(ra,rb)		   => (emit "cmp\t"; emitReg ra; comma();
				       emitReg rb)
          | FCMP(fp1,fp2)	   => (emit "fcmpo\tcr2, "; emitReg fp1; 
				       comma(); emitReg fp2)

          | AND	arg		   => (emit "and\t"; emit_2reg_ea arg)
          | OR arg		   => (emit "or\t"; emit_2reg_ea arg)
          | XOR	arg		   => (emit "xor\t"; emit_2reg_ea arg)
	  | XORU arg 		   => (emit "xoru\t"; emit_2reg_ea arg)

          | SL arg		   => (emit "sl\t"; emit_shift arg)
          | SRA arg		   => (emit "sr\t"; emit_shift arg)

          | FMR(fp1,fp2)	   => (emit "fmr\t"; emitReg fp1; comma();
				       emitReg fp2)
	  | MTFSB1 n		   => (emit "mtfsb1\t"; emit(makestring n))
	  | TRAP() 		   => (emit "teq\tr0,r0") );

       newLine();
       advance 4)
    end

  fun comment s = emit ("#" ^ s ^ "\n")

  fun init(n:int) = 
      (loc := 0; emit "## code size = "; emit (makestring n ^ " bytes\n"));
end


