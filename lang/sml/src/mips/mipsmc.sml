structure KeepMipsMCode : sig
			      val code : ByteArray.bytearray ref
			      val getCodeString : unit -> string
			      val cleanup : unit -> unit
			  end =
struct
    open ByteArray
    val code = ref (array(0,0))
    fun getCodeString () = let val s = extract (!code, 0, length (!code))
			   in 
			       code := array(0, 0); s
			   end
    fun cleanup () = code := array(0,0)
end
	
functor MipsMCode(structure E : ENDIAN) : EMITTER = struct

  structure M = MipsInstrSet 
  structure K = KeepMipsMCode
  open M

  val error = ErrorMsg.impossible

  val << = Bits.lshift
  val >> = Bits.rshift
  val || = Bits.orb
  val &  = Bits.andb
  infix << >> || &

  val loc = ref 0

  fun init n = (K.code := ByteArray.array(n, 0); loc := 0)

  fun emitByte n = let val i = !loc
		   in
		       loc := i + 1; ByteArray.update (!K.code, i, n)
		   end

  fun emitHiLo(hi,lo) = let
        val (byte0,byte1,byte2,byte3) = E.wordLayout (hi,lo)
      in  
	  emitByte byte0;
	  emitByte byte1;
	  emitByte byte2;
	  emitByte byte3
      end

  fun emitLong n = emitHiLo((n >> 16) & 65535, n & 65535)

  fun emitString s = let
	fun copy i = (emitByte(ordof(s, i)); copy(i+1))
	in
	  (copy 0) handle Ord => ()
	end

  exception BadReal = IEEEReal.BadReal
  val emitReal = emitString o E.order_real o IEEEReal.realconst

  fun emitAddr (INFO{addrOf,...}) (lab,k) = emitLong (k + addrOf lab - !loc)

  fun define _ _ = ()

  local open System.Tags
  in
      fun mark() = emitLong(make_desc((!loc + 4)>>2,tag_backptr))
  end

  fun comment _ = ()

  fun emitInstr info =
  let val labelValue = M.labelValue info
     val hiLabelValue = M.hiLabelValue info
     val loLabelValue = M.loLabelValue info
     val labBranchOff = M.labBranchOff info

    (* order of operands is identical to instr. format layout *)

      fun R_Type(opcode,rs',rt',rd',shamt,func) = 
	  case (reg_rep rs', reg_rep rt', reg_rep rd')
	   of  (Reg' rs, Reg' rt, Reg' rd) =>
	       emitHiLo((opcode << 10) || (rs << 5) || rt,
			(rd << 11) || (shamt << 6) || func)
	    | _ => error "MipsMCode.R_Type:"

      fun I_Type(opcode,rs',rt',immed) = 
	  case (reg_rep rs', reg_rep rt')
	   of (Reg' rs, Reg' rt) =>
	        emitHiLo((opcode << 10) || (rs << 5) || rt, immed)
	    | _ => error "MipsMCode.I_Type:"

      fun J_Type(opcode,target) = let
            val targetHi = (target >> 16) & 1023
	    val targetLo = target & 65535
	  in
	      emitHiLo((opcode << 10) || targetHi, targetLo)
	  end

      fun R_Type_f(opcode,format,ft',fs',fd',func) = 
	  case (reg_rep ft', reg_rep fs', reg_rep fd')
	   of (Freg' ft, Freg' fs, Freg' fd) =>
	       emitHiLo((opcode << 10) || (format << 5) ||  ft,
			(fs << 11) || (fd << 6) || func)
	    | _ => error "MipsMCode.R_Type_f"

      fun I_Type_f(opcode,base',ft',immed) = 
	  case (reg_rep base', reg_rep ft') 
	    of (Reg' base, Freg' ft) =>
	      emitHiLo((opcode << 10) || (base << 5) || ft, immed)
	     | _ => error "MipsMCode.I_Type_f:"	  
	  
      fun immediate_arith (Immed16Op n)    = M.chk_immed16 n
	| immediate_arith (LabelOp labexp) = M.chk_immed16(labelValue labexp)
	| immediate_arith (HiLabOp labexp) = hiLabelValue labexp
	| immediate_arith (LoLabOp labexp) = loLabelValue labexp
	| immediate_arith _ = error "MipsMCode.immediate_arith"

      fun immediate_mem (Immed16Off n) = M.chk_immed16 n
	| immediate_mem (LabOff labexp) = M.chk_immed16(labelValue labexp)
	| immediate_mem (HiLabOff labexp) = hiLabelValue labexp
	| immediate_mem (LoLabOff labexp) = loLabelValue labexp

      fun immediate_branch (opnd as LabOff labexp) = let
	    val labOff = labBranchOff opnd 
          in
	      labOff - ((!loc + 4) >> 2)
          end
	| immediate_branch _ = error "MipsMCode.immdiate_branch: bad label"
  in
      fn NOP 		    => emitHiLo(0,0)

       | SLT(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,42)
       | SLT(rt,rs,opnd)      => I_Type(10,rs,rt,immediate_arith opnd)
       | SLTU(rd,rs,RegOp rt) => R_Type(0,rs,rt,rd,0,43)
       | SLTU(rt,rs,opnd)     => I_Type(11,rs,rt,immediate_arith opnd)

       | SLT_DOUBLE(fs,ft)    => R_Type_f(17,17,ft,fs,Freg 0,60)
       | SEQ_DOUBLE(fs,ft)    => R_Type_f(17,17,ft,fs,Freg 0,58)

       | JUMP rs	       => R_Type(0,rs,Reg 0,Reg 0,0,0x8)
       | BLTZAL()              => I_Type(1,Reg 0,Reg 0x11,1)
       | BEQ(true,rs,rt,opnd)  => I_Type(0x4,rs,rt,immediate_branch opnd)
       | BEQ(false,rs,rt,opnd) => I_Type(0x5,rs,rt,immediate_branch opnd)
       | BCOP1(true, opnd)     => I_Type_f(17,Reg 8,Freg 1,immediate_branch opnd)
       | BCOP1(false, opnd)    => I_Type_f(17,Reg 8,Freg 0,immediate_branch opnd)

       | ADD(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x20)
       | ADD(rt,rs,opnd)      => I_Type(8,rs,rt,immediate_arith opnd)
       | ADDU(rd,rs,RegOp rt) => R_Type(0,rs,rt,rd,0,0x21)
       | ADDU(rt,rs,opnd)     => I_Type(9,rs,rt,immediate_arith opnd)
       | AND(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x24)
       | AND(rt,rs,opnd)      => I_Type(12,rs,rt,immediate_arith opnd)
       | OR(rd,rs,RegOp rt)   => R_Type(0,rs,rt,rd,0,0x25)
       | OR(rt,rs,opnd)       => I_Type(13,rs,rt,immediate_arith opnd)
       | XOR(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x26)
       | XOR(rt,rs,opnd)      => I_Type(14,rs,rt,immediate_arith opnd)
       | SUB(rd,rs,rt)        => R_Type(0,rs,rt,rd,0,0x22)

       | MULT(rs,rt)     => R_Type(0,rs,rt,Reg 0,0,0x18)
       | DIV(rs,rt)      => R_Type(0,rs,rt,Reg 0,0,0x1a)
       | MFHI rd         => R_Type(0,Reg 0,Reg 0,rd,0,0x10)
       | MFLO rd         => R_Type(0,Reg 0,Reg 0,rd,0,0x12)
       | BREAK n         => R_Type(0,Reg 0,Reg n,Reg 0,0,13)

       | ADD_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,0)
       | SUB_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,1)
       | MUL_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,2)
       | DIV_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,3)
       | MOV_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,6)
       | NEG_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,7)
       | ABS_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,5)
       | CVTI2D(fd,fs)        => R_Type_f(17,20,Freg 0,fs,fd,0x21)
       | MTC1(rt,fs)      => 
	          (case reg_rep rt
		    of Reg' rt' => R_Type_f(17,4, Freg rt',fs,Freg 0,0)
		     | _ => error "MipsMCode.emitInstr: MTC1")

       | LBU(rt,base,opnd)  => I_Type(0x24,base,rt,immediate_mem opnd)
       | SB(rt,base,opnd)   => I_Type(0x28,base,rt,immediate_mem opnd)
       | LW(rt,base,opnd)   => I_Type(0x23,base,rt,immediate_mem opnd)
       | SW(rt,base,opnd)   => I_Type(0x2b,base,rt,immediate_mem opnd)
       | LWC1(ft,base,opnd) => I_Type_f(0x31,base,ft,immediate_mem opnd)
       | SWC1(ft,base,opnd) => I_Type_f(0x39,base,ft,immediate_mem opnd)	
       | LUI(rt,opnd)       => I_Type(0xf,Reg 0,rt,immediate_mem opnd)

       | SLL(rd,rt,Int5 n) => R_Type(0,Reg 0,rt,rd,n,0)
       | SLLV(rd,rt,rs)    => R_Type(0,rs,rt,rd,0,4) 		(* weird! *)
       | SRA(rd,rt,Int5 n) => R_Type(0,Reg 0,rt,rd,n,3)
       | SRAV(rd,rt,rs)    => R_Type(0,rs,rt,rd,0,7) 		(* weird! *)
  end (* local *)

end


