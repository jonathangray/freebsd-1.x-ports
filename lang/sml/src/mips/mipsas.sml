structure MipsAsmStream = 
    struct
	val asmStream = ref std_out
    end

functor MipsAsCode() : EMITTER = 
struct

val error = ErrorMsg.impossible

structure M = MipsInstrSet
open M

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

fun emit s = output(!MipsAsmStream.asmStream,s)

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

fun emitAddr (info as INFO{addrOf,...}) (lab, k) =
    (emit "\t.long\t"; emitLabel info lab; 
     emit "\t## "; emit(itoa(addrOf lab - !loc)); 
     newLine(); advance 4)

fun define (info as INFO{addrOf,...}) lab =
    (emitLabel info lab; emit ":\t## "; emit(itoa (addrOf lab)); newLine())

local open System.Tags
in 
    fun mark() = (emit "\t.long\tMAKE_DESC(((.-L0)/4+1),tag_backptr)\t## ";
		  emit (itoa (make_desc((!loc+4)quot 4,tag_backptr))); 
		  newLine(); advance 4)
end 

fun emitInstr info =
 let val labelValue = M.labelValue info
     val hiLabelValue = M.hiLabelValue info
     val loLabelValue = M.loLabelValue info
     val labBranchOff = M.labBranchOff info
 in fn I =>
  let fun emitReg reg =
          emit ("$" ^ (case reg_rep reg 
	  	         of Reg' r =>  Integer.makestring r
		          | Freg' fp => "f" ^ Integer.makestring fp))

      fun comma() = emit ", "

      fun emit_arithOpnd (RegOp r) = emitReg r
	| emit_arithOpnd (Immed16Op i) = emit(makestring (chk_immed16 i))
	| emit_arithOpnd lab =  
	    emit(makestring 
		   (case lab
		      of LabelOp labexp => labelValue labexp
		       | HiLabOp labexp => hiLabelValue labexp
		       | LoLabOp labexp => loLabelValue labexp))

      fun emit_memOpnd (Immed16Off i) = emit(makestring (chk_immed16 i))
	| emit_memOpnd lab =
	    emit(makestring
		   (case lab
		      of LabOff labexp   => labelValue labexp
		       | HiLabOff labexp => hiLabelValue labexp
		       | LoLabOff labexp => loLabelValue labexp))

      fun emit_branchOpnd opnd =
	    emit(makestring(chk_immed16(labBranchOff opnd - (!loc + 4) div 4)))

      fun emit_arith(rd,rs,opnd) = 
	  (emitReg rd; comma(); emitReg rs; comma(); emit_arithOpnd opnd)
	
      fun emit_3regs(rd,rs,rt) = 
	  (emitReg rd; comma(); emitReg rs; comma(); emitReg rt)

      fun emit_2regs(rs,rt) = (emitReg rs; comma(); emitReg rt)

      fun emit_3float(fd,fs,ft) =
	  (emitReg fd; comma(); emitReg fs; comma(); emitReg ft)

      fun emit_2float(fs,ft) = (emitReg fs; comma(); emitReg ft)

      fun emit_mem(rd,base,offset) = 
	  (emitReg rd; comma(); emit_memOpnd offset; emit "(";
	   emitReg base; emit ")")

      fun emit_shift(rd,rt,Int5 i) = 
	  (emitReg rd; comma(); emitReg rt; comma(); emit(makestring i))

  in (emit "\t";
      (case I 
       of M.NOP 		=> emit "nop"

	| M.SLT arg 		=> (emit "slt\t"; emit_arith arg)
	| M.SLTU arg		=> (emit "sltu\t"; emit_arith arg)
	| M.SLT_DOUBLE arg	=> (emit "c.lt.d\t"; emit_2float arg)
	| M.SEQ_DOUBLE arg	=> (emit "c.eq.d\t"; emit_2float arg)

	| M.JUMP r      	=> (emit "jr\t"; emitReg r)
	| M.BLTZAL() 		=> (emit "bltzal\t")
	| M.BEQ(cond,r1,r2,offset)
	  			=> (emit(if cond then "beq\t" else "bne\t");
				    emitReg r1; comma();
				    emitReg r2; comma(); 
				    emit_branchOpnd offset)
	| M.BCOP1(cond,offset)  => (emit(if cond then "bc1t\t" else "bc1f\t");
				    emit_branchOpnd offset)

	| M.ADD arg		=> (emit "add\t"; emit_arith arg)
	| M.ADDU arg 		=> (emit "addu\t";emit_arith arg)
	| M.AND arg		=> (emit "and\t"; emit_arith arg)
	| M.OR arg		=> (emit "or\t";  emit_arith arg)
	| M.XOR arg		=> (emit "xor\t"; emit_arith arg)
	| M.SUB arg		=> (emit "sub\t"; emit_3regs arg)

	| M.MULT arg 	        => (emit "mult\t"; emit_2regs arg)
	| M.DIV arg		=> (emit "div\t"; emit_2regs arg)
	| M.MFLO r 		=> (emit "mflo\t"; emitReg r)
	| M.MFHI r 		=> (emit "mfhi\t"; emitReg r)
	| M.BREAK i 		=> (emit "break\t"; emit(makestring i))

	| M.MUL_DOUBLE arg	=> (emit "mul.d\t"; emit_3float arg)
	| M.DIV_DOUBLE arg	=> (emit "div.d\t"; emit_3float arg)
	| M.ADD_DOUBLE arg	=> (emit "add.d\t"; emit_3float arg)
	| M.SUB_DOUBLE arg	=> (emit "sub.d\t"; emit_3float arg)
	| M.NEG_DOUBLE arg	=> (emit "neg.d\t";   emit_2float arg)
	| M.ABS_DOUBLE arg	=> (emit "abs.d\t";   emit_2float arg)
	| M.CVTI2D arg          => (emit "cvt.d.s\t"; emit_2float arg)
	| M.MTC1 arg            => (emit "mtc1\t"; emit_2regs arg)

	| M.MOV_DOUBLE arg	=> (emit "mov.d\t"; emit_2float arg)

	| M.LBU arg 		=> (emit "lbu\t";  emit_mem arg)
	| M.SB arg 		=> (emit "sb\t";   emit_mem arg)
	| M.LW arg 		=> (emit "lw\t";   emit_mem arg)
	| M.SW arg 		=> (emit "sw\t";   emit_mem arg)
	| M.LWC1 arg 	        => (emit "lwc1\t"; emit_mem arg)
	| M.SWC1 arg 	        => (emit "swc1\t"; emit_mem arg)
	| M.LUI(r,mOff)         => (emit "lui\t"; emitReg r; comma();
				    emit_memOpnd mOff)
	| M.SLL arg		=> (emit "sll\t"; emit_shift arg)
	| M.SRA arg		=> (emit "sra\t"; emit_shift arg)
	| M.SLLV arg 		=> (emit "sllv\t"; emit_3regs arg)
        | M.SRAV arg 		=> (emit "srav\t"; emit_3regs arg));
     newLine();
     advance 4)
  end
 end

fun comment s = emit ("#" ^ s ^ "\n")

fun init(n:int) = 
    (loc := 0; emit "## code size = "; emit (makestring n ^ " bytes\n"));
end







