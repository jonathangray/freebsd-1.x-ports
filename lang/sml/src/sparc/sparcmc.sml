(* sparcmc.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * The SPARC machine code emitter.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *)

structure SparcMCode =
struct
    local open ByteArray in

    val code = ref (array(0, 0))

    fun getCodeString () = let
	  val s = extract (!code, 0, length (!code))
	  in
	    code := array(0, 0);
	    s
	  end

    end (* local *)
end (* SparcMCode *)

structure SparcMCEmit : EMITTER =
struct

    open SparcMCode SparcInstr

  (* Bit-wise operations *)
    val << = Bits.lshift
    val >> = Bits.rshift
    val ++ = Bits.orb
    val & = Bits.andb
    infix << >> ++ &

    val loc = ref 0     (* the location counter *)

    fun emitByte n = let
	  val i = !loc
	  in
	    loc := i+1;  ByteArray.update (!code, i, n)
	  end

    fun emitWord n = (emitByte((n >> 8) & 255); emitByte(n & 255))

    fun emitLong n = (emitWord(n >> 16); emitWord(n & 65535))

    fun emitString s = let
	  fun copy i = (emitByte(ordof(s, i)); copy(i+1))
	  in
	    (copy 0) handle Ord => ()
	  end

    exception BadReal = IEEEReal.BadReal
    fun emitReal s = emitString(IEEEReal.realconst s)

    fun emitAddr (INFO{addrOf,...}) (lab, k) = emitLong (k + addrOf lab - !loc)

    fun define _ _ = ()

    local
      open System.Tags
    in
    fun mark () = emitLong (make_desc((!loc + 4) >> 2, tag_backptr))
    end (* local *)

    fun emitInstr (info as INFO{addrOf,...}) = let
	  fun valOf (LABELexp{base, dst, offset}) =
		((addrOf dst) + offset) - (addrOf base)
	  fun immed13 (IMrand i) = (i & 8191)                     (* 13 bits *)
	    | immed13 (LABrand labexp) = ((valOf labexp) & 8191)  (* 13 bits *)
	    | immed13 (LOrand labexp) = ((valOf labexp) & 1023)   (* 10 bits *)
	    | immed13 _ = ErrorMsg.impossible "[SparcMCEmit.immed13]"
	(* emit a 3 operand instruction with "11" in bits 31-30 *)
	  fun emitOp11 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(49152 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ b))
	    | emitOp11 opcode (REG a, b, REG c) = (
		emitWord(49152 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ 8192 ++ (immed13 b)))

	  val emit_ld = emitOp11 0
	  val emit_st = emitOp11 4
	  val emit_ldb = emitOp11 1
	  val emit_stb = emitOp11 5
	  fun emit_ldf (r, ri, FREG fr) = emitOp11 32 (r, ri, REG fr)
	  fun emit_stf (r, ri, FREG fr) = emitOp11 36 (r, ri, REG fr)

	(* emit a branch instruction *)
	  fun emitBcc opcode lab = let
		val disp = (((addrOf lab) - !loc) >> 2)
		val d = if disp < 0 then (disp + 4194304) else disp
		in
		  if ((disp < ~2097152) orelse (2097152 <= disp))
		    then ErrorMsg.impossible "[SparcMCEmit.emitBcc]" else ();
		  emitLong ((opcode << 22) ++ d)
		end

	  val emit_ba = emitBcc 66        (* 1000010 *)
	  val emit_be = emitBcc 10        (* 0001010 *)
	  val emit_bne = emitBcc 74       (* 1001010 *)
	  val emit_ble = emitBcc 18       (* 0010010 *)
	  val emit_bge = emitBcc 90       (* 1011010 *)
	  val emit_bl = emitBcc 26        (* 0011010 *)
	  val emit_bg = emitBcc 82        (* 1010010 *)
	  val emit_bgeu = emitBcc 106     (* 1101010 *)
	  val emit_bleu = emitBcc 34      (* 0100010 *)
	  val emit_fbe = emitBcc 78       (* 1001110 *)
	  val emit_fbne = emitBcc 14      (* 0001110 *)
	  val emit_fble = emitBcc 110     (* 1101110 *)
	  val emit_fbge = emitBcc 94      (* 1011110 *)
	  val emit_fbl = emitBcc 38       (* 0100110 *)
	  val emit_fbg = emitBcc 54       (* 0110110 *)

	(* emit a 3 operand instructions with "10" in bits 31-30. *)
	  fun emitOp10 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(32768 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ b))
	    | emitOp10 opcode (REG a, b, REG c) = (
		emitWord(32768 ++ (c << 9) ++ (opcode << 3) ++ (a >> 2));
		emitWord(((a & 3) << 14) ++ 8192 ++ (immed13 b)))

	  val emit_jmpl = emitOp10 56       (* 111000 *)

	  fun emit_call2 () = (		(* opcode = 0x40000002 *)
		emitByte 64; emitByte 0; emitByte 0; emitByte 2)
	(* integer operations *)
	  val emit_add = emitOp10 0         (* 000000 *)
	  val emit_addcc = emitOp10 16      (* 010000 *)
	  val emit_taddcctv = emitOp10 34   (* 100010 *)
	  val emit_sub = emitOp10 4         (* 000100 *)
	  val emit_subcc = emitOp10 20      (* 010100 *)
	  val emit_sll = emitOp10 37        (* 100101 *)
	  val emit_sra = emitOp10 39        (* 100111 *)
	  val emit_and = emitOp10 1         (* 000001 *)
	  val emit_andcc = emitOp10 17      (* 010001 *)
	  val emit_or = emitOp10 2          (* 000010 *)
	  val emit_xor = emitOp10 3         (* 000011 *)
	  val emit_xnor = emitOp10 7        (* 000111 *)

	(* emit a FOp1 floating-point instruction of three args; this has "10" in
	 * bits 31-30 and "110100" in bits 24-19.
	 *)
	  fun emitFOp1_3 opcode (FREG a, FREG b, FREG c) = (
		emitWord (33184 ++ (c << 9) ++ (a >> 2));
		emitWord (((a & 3) << 14) ++ (opcode << 5) ++ b))
	(* emit a FOp1 floating-point instruction of two args (same bits as above) *)
	  fun emitFOp1_2 opcode (FREG a, FREG b) = (
		emitWord (33184 ++ (b << 9));
		emitWord ((opcode << 5) ++ a))
	(* emit a FOp2 floating-point instruction of two args.  This has "10" in
	 * bits 31-30 and "110101" in bits 24-19.
	 *)
	  fun emitFOp2_2 opcode (FREG a, FREG b) = (
		emitWord (33192 ++ (a >> 2));
		emitWord (((a & 3) << 14) ++ (opcode << 5) ++ b))
	  val emit_fadd  = emitFOp1_3 0x042    (* 0 0100 0010 *)
	  val emit_fsub  = emitFOp1_3 0x046    (* 0 0100 0110 *)
	  val emit_fmul  = emitFOp1_3 0x04a    (* 0 0100 1010 *)
	  val emit_fdiv  = emitFOp1_3 0x04e    (* 0 0100 1110 *)
	  val emit_fneg  = emitFOp1_2 0x005    (* 0 0000 0101 *)
	  val emit_fabs  = emitFOp1_2 0x009    (* 0 0000 1001 *)
	  val emit_fcmp  = emitFOp2_2 0x052    (* 0 0101 0010 *)
	  val emit_fmov  = emitFOp1_2 0x001    (* 0 0000 0001 *)
	  val emit_fitod = emitFOp1_2 0x0c8    (* 0 1100 1000 *)
	  in

	    fn I_nop => emitLong 16777216 (* really "sethi 0,%g0" *)
	     | I_ld args => emit_ld args
	     | I_ldb args => emit_ldb args
	     | I_ldf args => emit_ldf args
	     | I_st args => emit_st args
	     | I_stb args => emit_stb args
	     | I_stf args => emit_stf args
	     | I_sethi(arg, REG rd) => let
		val im = case arg
		     of (IMrand i) => i
		      | (HIrand labexp) => ((valOf labexp) >> 10)
		      | _ => ErrorMsg.impossible "[SparcMCEmit.emitInstr:sethi]"
		in
		  emitWord(256 ++ (rd << 9) ++ ((im >> 16) & 63));
		  emitWord(im & 65535)
		end
	     | I_bcc(CC_A, lab) => emit_ba lab
	     | I_bcc(CC_E, lab) => emit_be lab
	     | I_bcc(CC_NE, lab) => emit_bne lab
	     | I_bcc(CC_L, lab) => emit_bl lab
	     | I_bcc(CC_LE, lab) => emit_ble lab
	     | I_bcc(CC_G, lab) => emit_bg lab
	     | I_bcc(CC_GE, lab) => emit_bge lab
	     | I_bcc(CC_GEU, lab) => emit_bgeu lab
	     | I_bcc(CC_LEU, lab) => emit_bleu lab
	     | I_fbcc(CC_E, lab) => emit_fbe lab
	     | I_fbcc(CC_NE, lab) => emit_fbne lab
	     | I_fbcc(CC_L, lab) => emit_fbl lab
	     | I_fbcc(CC_LE, lab) => emit_fble lab
	     | I_fbcc(CC_G, lab) => emit_fbg lab
	     | I_fbcc(CC_GE, lab) => emit_fbge lab
	     | I_jmpl args => emit_jmpl args
	     | I_call2 => emit_call2()
	     | I_add args => emit_add args
	     | I_addcc args => emit_addcc args
	     | I_taddcctv args => emit_taddcctv args
	     | I_sub args => emit_sub args
	     | I_subcc args => emit_subcc args
	     | I_sll args => emit_sll args
	     | I_sra args => emit_sra args
	     | I_and args => emit_and args
	     | I_andcc args => emit_andcc args
	     | I_or args => emit_or args
	     | I_xor args => emit_xor args
	     | I_not(r1, rd) => emit_xnor (r1, REGrand(REG 0), rd)
	     | I_tvs => (emitWord 36816; emitWord 8199)  (* "tvs 0x7" *)
	     | I_fadd args => emit_fadd args
	     | I_fsub args => emit_fsub args
	     | I_fmul args => emit_fmul args
	     | I_fdiv args => emit_fdiv args
	     | I_fneg args => emit_fneg args
	     | I_fabs args => emit_fabs args
	     | I_fcmp args => emit_fcmp args
	     | I_fmov args => emit_fmov args
	     | I_fitod args => emit_fitod args
	     | _ => ErrorMsg.impossible "[SparcMCEmit.emitInstr]"

    end (* emitInstr *)

    fun comment _ = ()

    fun init n = (code := ByteArray.array(n, 0); loc := 0)

end (* structure SparcMCEmit *)
