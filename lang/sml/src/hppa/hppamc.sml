(* Copyright 1992 Scott Draves *)

structure HppaMCode =
struct
    local
	open ByteArray
    in
	val code = ref (array(0, 0))

	fun getCodeString () =
	    let val s = extract (!code, 0, length (!code))
	    in  code := array(0, 0);
	        s
	    end

    end (* local *)
end (* HppaMCode *)

structure HppaMCEmit : EMITTER =
struct

    open HppaMCode HppaInstr

  (* Bit-wise operations *)
    val << = Bits.lshift
    val >> = Bits.rshift
    val ++ = Bits.orb
    val & = Bits.andb
    infix << >> ++ &

    val loc = ref 0     (* the location counter *)

    fun emitByte n =
	let val i = !loc
	in  loc := i+1;
	    ByteArray.update (!code, i, n)
	end

    fun emitWord n = (emitByte((n >> 8) & 255); emitByte(n & 255))

    fun emitLong n = (emitWord(n >> 16); emitWord(n & 65535))

    fun emitString s = let
	  fun copy i = (emitByte(ordof(s, i)); copy(i+1))
	  in
	    (copy 0) handle Ord => ()
	  end

    (* all hppa instructions consist of a 6 bit identifier, followed by
       the 26 bit instruction specific body. *)
    fun emit(id, body) =
	let val b1 = (id << 2) ++ (body >> 24)
	    and b2 = (body >> 16) & 255
	    and b3 = (body >> 8) & 255
	    and b4 = body & 255
	    and i = !loc
	in
	    ByteArray.update(!code, i  , b1);
	    ByteArray.update(!code, i+1, b2);
	    ByteArray.update(!code, i+2, b3);
	    ByteArray.update(!code, i+3, b4);
	    loc := i+4
	end handle _ => ErrorMsg.impossible "HppaMCEmit.emit range"

    exception BadReal = IEEEReal.BadReal
    fun emitReal s = emitString(IEEEReal.realconst s)

    fun emitAddr (info as INFO{addrOf, ...}) (lab, k) =
	emitLong (k + addrOf(lab) - !loc)

    fun define _ _ = ()

    local
	open System.Tags
    in
	fun mark () = emitLong (make_desc((!loc + 4) >> 2, tag_backptr))
    end

    local
	fun ms2u ms_none   = 0
	  | ms2u ms_modify = 0
	  | ms2u _         = 1

	fun ms2m ms_shift = 0
	  | ms2m ms_none  = 0
	  | ms2m _        = 1

	fun mab2a mab_before = 1
	  | mab2a _          = 0

	fun mab2m mab_none = 0
	  | mab2m _        = 1

	fun mbe2a mbe_begin        = 0
	  | mbe2a mbe_begin_modify = 0
	  | mbe2a _                = 1

	fun mbe2m mbe_begin = 0
	  | mbe2m mbe_end   = 0
          | mbe2m _         = 1

        fun nullify2b n_nullify = 1
	  | nullify2b _         = 0

	fun arithcond2c cc_never                  = 0
	  | arithcond2c cc_equal                  = 1
	  | arithcond2c cc_less                   = 2
	  | arithcond2c cc_less_or_equal          = 3
	  | arithcond2c cc_less_unsigned          = 4
	  | arithcond2c cc_less_or_equal_unsigned = 5
	  | arithcond2c cc_signed_overflow        = 6
	  | arithcond2c cc_odd                    = 7

	fun arithcond22c (cc x)     = arithcond2c(x)
	  | arithcond22c (cc_not x) = arithcond2c(x)

	fun arithcond22f (cc x)     = 0
	  | arithcond22f _          = 1

	fun logicond2c lc_never                      = 0
	  | logicond2c lc_all_zero                   = 1
	  | logicond2c lc_leftmost_one               = 2
	  | logicond2c lc_leftmost_one_or_all_zero   = 3
	  | logicond2c lc_rightmost_one              = 7
	  | logicond2c lc_always                     = 0
	  | logicond2c lc_some_one                   = 1
	  | logicond2c lc_leftmost_zero              = 2
	  | logicond2c lc_leftmost_zero_and_some_one = 3
	  | logicond2c lc_rightmost_zero             = 7

	fun logicond2f lc_never                      = 0
	  | logicond2f lc_all_zero                   = 0
	  | logicond2f lc_leftmost_one               = 0
	  | logicond2f lc_leftmost_one_or_all_zero   = 0
	  | logicond2f lc_rightmost_one              = 0
	  | logicond2f _                             = 1

	fun unitcond2c uc_never               = 0
	  | unitcond2c uc_some_byte_zero      = 2
	  | unitcond2c uc_some_halfword_zero  = 3
	  | unitcond2c uc_some_digit_carry    = 4
	  | unitcond2c uc_some_byte_carry     = 6
	  | unitcond2c uc_some_halfword_carry = 7
	    
	fun unitcond22c (uc x)     = unitcond2c(x)
	  | unitcond22c (uc_not x) = unitcond2c(x)

	fun unitcond22f (uc x)     = 0
	  | unitcond22f _          = 1

	fun shiftcond2b sc_never          = 0
	  | shiftcond2b sc_all_zero       = 1
	  | shiftcond2b sc_leftmost_one   = 2
	  | shiftcond2b sc_rightmost_one  = 3
	  | shiftcond2b sc_always         = 4
	  | shiftcond2b sc_some_one       = 5
	  | shiftcond2b sc_leftmost_zero  = 6
	  | shiftcond2b sc_rightmost_zero = 7
	    
	fun fpf2b fpf_single = 0
	  | fpf2b fpf_double = 1
	  | fpf2b fpf_quad   = 3 (* no typo *)

	fun fpcond2b (fpc {less, greater, equal, unordered, trap}) =
	    let val n =     (if trap      then  1 else 0)
		val n = n + (if unordered then  2 else 0)
		val n = n + (if equal     then  4 else 0)
		val n = n + (if less      then  8 else 0)
		val n = n + (if greater   then 16 else 0)
	    in n
	    end

	fun fphalf FrLeftHalf  = 0
	  | fphalf FrRightHalf = 1

	(* should do bounds check here for added protection? XXX *)
	fun bits5  n = n &      31
	fun bits11 n = n &    2047
	fun bits12 n = n &    4095
	fun bits13 n = n &    8191
	fun bits14 n = n &   16383
	fun bits17 n = n &  131071
	fun bits21 n = n & 2097151

	(* these put the sign bit last instead of first *)
	fun bits5low  n = ((n &    0xf) << 1) ++ ((n &   0x10) >>  4)
	fun bits11low n = ((n &  0x3ff) << 1) ++ ((n &  0x400) >> 10)
	fun bits14low n = ((n & 0x1fff) << 1) ++ ((n & 0x2000) >> 13)

	fun frag3 n = ((n & 1) << 2) ++ ((n & 5) >> 1)

        fun frag21 n = (((n & 0x000003) << 12) ++
			((n & 0x00007c) << 14) ++
			((n & 0x000180) << 7) ++
			((n & 0x0ffe00) >> 8) ++
			((n & 0x100000) >> 20))

	fun frag17_0 n = (n & 0x10000) >> 16
	fun frag17_1 n = (n & 0x0f800) >> 11
	fun frag17_2 n = (((n & 0x3ff) << 1) ++
			  ((n & 0x400) >> 10))

	fun frag12_0 n = (n & 0x800) >> 11
	fun frag12_1 n = (((n & 0x400) >> 10) ++
			  ((n & 0x3ff) << 1))

	fun emit_mem_offset(code, (REG base, im, REG t)) =
	    emit(code, (base << 21) ++ (t << 16) ++ bits14low(im))

	fun emit_mem_index(code, size, (REG r1, REG r2, ms, REG t)) =
	    emit(code,
		 (r1 << 21) ++ (r2 << 16) ++
		 (ms2u(ms) << 13) ++ (ms2m(ms) << 5) ++
		 t ++ (size << 6))

	fun emit_ld_short(code, size, (REG r1, im, mab, REG t)) =
	    emit(code,
		  (r1 << 21) ++ (bits5low(im) << 16) ++
		 (mab2a(mab) << 13) ++ (mab2m(mab) << 5) ++
		 t ++ (size << 6) ++ (1 << 12))

	fun emit_st_short(code, size, (REG r1, im, mab, REG t)) =
	    emit(code,
		  (r1 << 21) ++ (t << 16) ++
		 (mab2a(mab) << 13) ++ (mab2m(mab) << 5) ++
		 bits5low(im) ++ (size << 6) ++ (1 << 12))

	fun emit_st_bytes(code, size, (REG r1, im, mbe, REG t)) =
	    emit(code,
		 (r1 << 21) ++ (t << 16) ++
		 (mbe2a(mbe) << 13) ++ (mbe2m(mbe) << 5) ++
		 bits5low(im) ++ (size << 6) ++ (1 << 12))

	fun emit_branch(code, b, (im, n, REG t)) =
	    let val im = bits17(im)
	    in emit(code,
		    (t << 21) ++ (frag17_1(im) << 16) ++
		    (b << 13) ++ (frag17_2(im) << 2) ++
		    (nullify2b(n) << 1) ++ frag17_0(im))
	    end

	fun emit_branchs(code, (im, n, SREG s, t)) =
	    emit_branch(code, frag3(s), (im, n, t))

	fun emit_branch2(code, b, (REG x, n, REG t)) =
	    emit(code,
		 (t << 21) ++ (x << 16) ++
		 (b << 13) ++ (nullify2b(n) << 1))

	fun emit_branch3(code, (i, REG r, cond, im, n)) =
	    let val i = bits5low(i)
		and im = bits12(im)
	    in emit(code,
		    (r << 21) ++ (i << 16) ++
		    (arithcond2c(cond) << 13) ++
		    (frag12_1(im) << 2) ++ (nullify2b(n) << 1) ++
		    frag12_0(im))
	    end

	fun emit_branch3r(code, (REG r1, REG r2, cond, im, n)) =
	    let val im = bits12(im)
	    in emit(code,
		    (r2 << 21) ++ (r1 << 16) ++
		    (arithcond2c(cond) << 13) ++
		    (frag12_1(im) << 2) ++ (nullify2b(n) << 1) ++
		    frag12_0(im))
	    end

	fun rrr(b, REG r1, REG r2, REG t) =
	    ((r2 << 21) ++ (r1 << 16) ++
	     (b << 5) ++ t)

	fun emit_add_rrr(code, b, (r1, r2, cond, t)) =
	    emit(code,
		 rrr(b, r1, r2, t) ++
		 (arithcond22c(cond) << 13) ++
		 (arithcond22f(cond) << 12))

	fun emit_log_rrr(code, b, (r1, r2, cond, t)) =
	    emit(code,
		 rrr(b, r1, r2, t) ++
		 (logicond2c(cond) << 13) ++
		 (logicond2f(cond) << 12))

	fun emit_unit_rrr(code, b, (r1, r2, cond, t)) =
	    emit(code,
		 rrr(b, r1, r2, t) ++
		 (unitcond22c(cond) << 13) ++
		 (unitcond22f(cond) << 12))

	fun emit_unit_rr(code, b, (r, t, cond)) =
	    emit(code,
		 rrr(b, REG 0, r, t) ++
		 (unitcond22c(cond) << 13) ++
		 (unitcond22f(cond) << 12))

	fun emit_add_irr(code, b, (im, REG r, cond, REG t)) =
	    emit(code,
		 (r << 21) ++ (t << 16) ++
		 (arithcond22c(cond) << 13) ++
		 (arithcond22f(cond) << 12) ++
		 (b << 11) ++ bits11low(im))

	fun emit_shift(code, b1, b2, r1, r2, cond, clen) =
	    emit(code,
		 (r1 << 21) ++ (r2 << 16) ++
		 (shiftcond2b(cond) << 13) ++
		 (b1 << 10) ++ (b2 << 5) ++ (32 - clen))

	fun emit_control(b, r1, r2, r3, s) =
	    emit(0, (r1 << 21) ++ (r2 << 16) ++
		 (frag3(s) << 13) ++ r3 ++ (b << 5))

	fun emit_fld_index(code, b, (REG r, REG x, ms, FREG t)) =
	    emit(code, (r << 21) ++ (x << 16) + t ++ (b << 9) ++
		 (ms2u(ms) << 13) ++ (ms2m(ms) << 5))

	fun emit_fld_short(code, b, (REG r, im, mab, FREG t)) =
	    emit(code, (r << 21) ++ (bits5low(im) << 16) ++ t ++ (b << 9) ++
		 (mab2a(mab) << 13) ++ (mab2m(mab) << 5) ++ (1 << 12))


	fun emit_fldw_index(code, b, (REG r, REG x, ms, FREG t, half)) =
	    emit(code, (r << 21) ++ (x << 16) + t ++ (b << 9) ++
		 (ms2u(ms) << 13) ++ (ms2m(ms) << 5) ++ 
		 (fphalf half << 6))

	fun emit_fldw_short(code, b, (REG r, im, mab, FREG t, half)) =
	    emit(code, (r << 21) ++ (bits5low(im) << 16) ++ t ++ (b << 9) ++
		 (mab2a(mab) << 13) ++ (mab2m(mab) << 5) ++ (1 << 12) ++
		 (fphalf half << 6))

	fun emit_freg_freg(b, (FREG r, fpf, FREG t)) =
	    emit(0x0c, (r << 21) ++ t ++ (b << 13) ++ (fpf2b(fpf) << 11))

	fun emit_cnv(b, (FREG r, fpf1, FREG t, fpf2)) =
	    emit(0x0c, (r << 21) ++ t ++ (b << 15) ++ (1 << 9) ++
		 (fpf2b(fpf1) << 11) ++ (fpf2b(fpf2) << 13))

	fun emit_fop(b, (FREG r1, FREG r2, fpf, FREG t)) =
	    emit(0x0c, (r1 << 21) ++ (r2 << 16) ++ t ++
		 (b << 13) ++ (3 << 9) ++ (fpf2b(fpf) << 11))

	fun emit_fop2(code, (FREG r1, FREG r2, FREG r3, FREG r4, FREG r5)) =
	    emit(code, (r1 << 21) ++ (r2 << 16) ++ (r3 << 11) ++
		 (r4 << 6) ++ r5)

    in
	(* all space codes are assumed to be 0
	   all cache hints are assumed to be 0 *)

	fun emitInstr info I =
	    case I of
		(i_nop) => emitInstr(info)(i_addl(zero_reg, zero_reg, never, zero_reg))
	      |  (i_ldw(x))  => emit_mem_offset(0x12, x)
	      |  (i_ldh(x))  => emit_mem_offset(0x11, x)
	      |  (i_ldb(x))  => emit_mem_offset(0x10, x)
	      |  (i_stw(x))  => emit_mem_offset(0x1a, x)
	      |  (i_sth(x))  => emit_mem_offset(0x19, x)
	      |  (i_stb(x))  => emit_mem_offset(0x18, x)
	      |  (i_ldwm(x)) => emit_mem_offset(0x13, x)
	      |  (i_stwm(x)) => emit_mem_offset(0x1b, x)

	      |  (i_ldwx(x))  => emit_mem_index(0x03, 0x2, x)
	      |  (i_ldhx(x))  => emit_mem_index(0x03, 0x1, x)
	      |  (i_ldbx(x))  => emit_mem_index(0x03, 0x0, x)
	      |  (i_ldwax(x)) => emit_mem_index(0x03, 0x6, x)
	      |  (i_ldcwx(x)) => emit_mem_index(0x03, 0x7, x)
	    
	      |  (i_ldws(x))  => emit_ld_short(0x03, 0x2, x)
	      |  (i_ldhs(x))  => emit_ld_short(0x03, 0x1, x)
	      |  (i_ldbs(x))  => emit_ld_short(0x03, 0x0, x)
	      |  (i_ldwas(x)) => emit_ld_short(0x03, 0x6, x)
	      |  (i_ldcws(x)) => emit_ld_short(0x03, 0x7, x)
	      |  (i_stws(x))  => emit_st_short(0x03, 0xa, x)
	      |  (i_sths(x))  => emit_st_short(0x03, 0x9, x)
	      |  (i_stbs(x))  => emit_st_short(0x03, 0x8, x)
	      |  (i_stwas(x)) => emit_st_short(0x03, 0xe, x)

	      |  (i_stbys(x)) => emit_st_bytes(0x03, 0xc, x)
	    
	      |  (i_ldo(REG r1, im, REG t)) =>
		     emit(0x0d, (r1 << 21) ++ (t << 16) ++ bits14low(im))

	      |  (i_ldil(im, REG t)) =>
		     emit(0x08, (t << 21) ++ frag21(bits21(im)))
					
	      |  (i_addil(im, REG t)) =>
		     emit(0x0a, (t << 21) ++ frag21(bits21(im)))

	      |  (i_bl(x))     => emit_branch(0x3a, 0, x)
	      |  (i_gate(x))   => emit_branch(0x3a, 1, x)
	      |  (i_blr(x))    => emit_branch2(0x3a, 2, x)
	      |  (i_bv(x))     => emit_branch2(0x3a, 6, x)
	      |  (i_be(x))     => emit_branchs(0x38, x)
	      |  (i_ble(x))    => emit_branchs(0x39, x)
	      |  (i_movb(x))   => emit_branch3r(0x32, x)
	      |  (i_movib(x))  => emit_branch3(0x33, x)
	      |  (i_combt(x))  => emit_branch3r(0x20, x)
	      |  (i_combf(x))  => emit_branch3r(0x22, x)
	      |  (i_comibt(x)) => emit_branch3(0x21, x)
	      |  (i_comibf(x)) => emit_branch3(0x23, x)
	      |  (i_addbt(x))  => emit_branch3r(0x28, x)
	      |  (i_addbf(x))  => emit_branch3r(0x2a, x)
	      |  (i_addibt(x)) => emit_branch3(0x29, x)
	      |  (i_addibf(x)) => emit_branch3(0x2b, x)

	      |  (i_bvb(REG r1, cond, im, n)) =>
		     let val im = bits12(im)
		     in emit(0x30, (r1 << 16) ++ (shiftcond2b(cond) << 13) ++
			     (frag12_1(im) << 2) ++ (nullify2b(n) << 1) ++
			     frag12_0(im))
		     end

	      |  (i_bb(REG r1, p, cond, im, n)) =>
		     let val im = bits12(im)
			 and p = bits5(p)
		     in emit(0x31, (r1 << 16) ++ (shiftcond2b(cond) << 13) ++
			     (frag12_1(im) << 2) ++ (nullify2b(n) << 1) ++
			     frag12_0(im) ++ (p << 21))
		     end

	      |  (i_add(x))     => emit_add_rrr(0x02, 0x30, x)
	      |  (i_addl(x))    => emit_add_rrr(0x02, 0x50, x)
	      |  (i_addo(x))    => emit_add_rrr(0x02, 0x70, x)
	      |  (i_addc(x))    => emit_add_rrr(0x02, 0x38, x)
	      |  (i_addco(x))   => emit_add_rrr(0x02, 0x78, x)
	      |  (i_sh1add(x))  => emit_add_rrr(0x02, 0x32, x)
	      |  (i_sh1addl(x)) => emit_add_rrr(0x02, 0x52, x)
	      |  (i_sh1addo(x)) => emit_add_rrr(0x02, 0x72, x)
	      |  (i_sh2add(x))  => emit_add_rrr(0x02, 0x34, x)
	      |  (i_sh2addl(x)) => emit_add_rrr(0x02, 0x54, x)
	      |  (i_sh2addo(x)) => emit_add_rrr(0x02, 0x74, x)
	      |  (i_sh3add(x))  => emit_add_rrr(0x02, 0x36, x)
	      |  (i_sh3addl(x)) => emit_add_rrr(0x02, 0x56, x)
	      |  (i_sh3addo(x)) => emit_add_rrr(0x02, 0x76, x)
	      |  (i_sub(x))     => emit_add_rrr(0x02, 0x20, x)
	      |  (i_subo(x))    => emit_add_rrr(0x02, 0x60, x)
	      |  (i_subb(x))    => emit_add_rrr(0x02, 0x28, x)
	      |  (i_subbo(x))   => emit_add_rrr(0x02, 0x68, x)
	      |  (i_subt(x))    => emit_add_rrr(0x02, 0x26, x)
	      |  (i_subto(x))   => emit_add_rrr(0x02, 0x66, x)
	      |  (i_ds(x))      => emit_add_rrr(0x02, 0x22, x)
	      |  (i_comclr(x))  => emit_add_rrr(0x02, 0x44, x)

	      |  (i_or(x))      => emit_log_rrr(0x02, 0x12, x)
	      |  (i_xor(x))     => emit_log_rrr(0x02, 0x14, x)
	      |  (i_and(x))     => emit_log_rrr(0x02, 0x10, x)
	      |  (i_andcm(x))   => emit_log_rrr(0x02, 0x00, x)

	      |  (i_uxor(x))    => emit_unit_rrr(0x02, 0x1c, x)
	      |  (i_uaddcm(x))  => emit_unit_rrr(0x02, 0x4c, x)
	      |  (i_uaddcmt(x)) => emit_unit_rrr(0x02, 0x4e, x)

	      |  (i_dcor(x))    => emit_unit_rr(0x02, 0x5c, x)
	      |  (i_idcor(x))   => emit_unit_rr(0x02, 0x5e, x)

	      |  (i_addi(x))    => emit_add_irr(0x2d, 0, x)
	      |  (i_addio(x))   => emit_add_irr(0x2d, 1, x)
	      |  (i_addit(x))   => emit_add_irr(0x2c, 0, x)
	      |  (i_addito(x))  => emit_add_irr(0x2c, 1, x)
	      |  (i_subi(x))    => emit_add_irr(0x25, 0, x)
	      |  (i_subio(x))   => emit_add_irr(0x25, 1, x)
	      |  (i_comiclr(x)) => emit_add_irr(0x24, 0, x)

	      |  (i_vshd(REG r1, REG r2, cond, REG t)) =>
		     emit_shift(0x34, 0, 0, r2, r1, cond, 32-t)
	      |  (i_shd(REG r1, REG r2, cond, cp, REG t)) =>
		     emit_shift(0x34, 2, cp, r2, r1, cond, 32-t)

	      |  (i_vextru(REG r, cond, len, REG t)) =>
		     emit_shift(0x34, 4, 0, r, t, cond, len)
	      |  (i_vextrs(REG r, cond, len, REG t)) =>
		     emit_shift(0x34, 5, 0, r, t, cond, len)
	      |  (i_extru(REG r, cond, start, len, REG t)) =>
		     emit_shift(0x34, 6, start, r, t, cond, len)
	      |  (i_extrs(REG r, cond, start, len, REG t)) =>
		     emit_shift(0x34, 7, start, r, t, cond, len)

	      |  (i_vdep(REG r, cond, len, REG t)) =>
		     emit_shift(0x35, 1, 0, t, r, cond, len)
	      |  (i_dep(REG r, cond, start, len, REG t)) =>
		     emit_shift(0x35, 3, 31-start, t, r, cond, len)
	      |  (i_vdepi(im, cond, len, REG t)) =>
		     emit_shift(0x35, 5, 0, t, bits5low(im), cond, len)
	      |  (i_depi(im, cond, start, len, REG t)) =>
		     emit_shift(0x35, 7, 31-start, t, bits5low(im), cond, len)
	    
	      |  (i_zvdep(REG r, cond, len, REG t)) =>
		     emit_shift(0x35, 0, 0, t, r, cond, len)
	      |  (i_zdep(REG r, cond, start, len, REG t)) =>
		     emit_shift(0x35, 2, 31-start, t, r, cond, len)
	      |  (i_zvdepi(im, cond, len, REG t)) =>
		     emit_shift(0x35, 4, 0, t, bits5low(im), cond, len)
	      |  (i_zdepi(im, cond, start, len, REG t)) =>
		     emit_shift(0x35, 6, 31-start, t, bits5low(im), cond, len)

	      | (i_break(im1, im2)) => emit(0, bits5(im1) ++ (bits13(im2) << 13))
	      | (i_rfi)  => emit_control(0x60, 0, 0, 0, 0)
	      | (i_rfir) => emit_control(0x65, 0, 0, 0, 0)
	      | (i_ssm(im, REG r)) => emit_control(0x6b, 0, bits5(im), r, 0)
	      | (i_rsm(im, REG r)) => emit_control(0x73, 0, bits5(im), r, 0)
	      | (i_mtsm(REG r)) => emit_control(0xc3, 0, r, 0, 0)
	      | (i_mtctl(REG r, CREG c)) => emit_control(0xc2, c, r, 0, 0)
	      | (i_mfctl(CREG c, REG r)) => emit_control(0x45, c, 0, r, 0)
	      | (i_ldsid(REG r, REG t)) => emit_control(0x85, r, 0, t, 0)
	      | (i_mtsp(REG r, SREG s)) => emit_control(0xc1, 0, r, 0, s)
	      | (i_mfsp(SREG s, REG r)) => emit_control(0x25, 0, 0, r, s)
	      | (i_sync) => emit_control(0x20, 0, 0, 0, 0)

	      | (i_fldwx(x)) => emit_fldw_index(0x09, 0, x)
	      | (i_fstwx(x)) => emit_fldw_index(0x09, 1, x)
	      | (i_fldws(x)) => emit_fldw_short(0x09, 0, x)
	      | (i_fstws(x)) => emit_fldw_short(0x09, 1, x)

	      | (i_flddx(x)) => emit_fld_index(0x0b, 0, x)
	      | (i_fstdx(x)) => emit_fld_index(0x0b, 1, x)
	      | (i_fldds(x)) => emit_fld_short(0x0b, 0, x)
	      | (i_fstds(x)) => emit_fld_short(0x0b, 1, x)

	      | (i_fcpy(x))  => emit_freg_freg(2, x)
	      | (i_fabs(x))  => emit_freg_freg(3, x)
	      | (i_fsqrt(x)) => emit_freg_freg(4, x)
	      | (i_frnd(x))  => emit_freg_freg(5, x)
	    
	      | (i_fcnvff(x))  => emit_cnv(0, x)
	      | (i_fcnvxf(x))  => emit_cnv(1, x)
	      | (i_fcnvfx(x))  => emit_cnv(2, x)
	      | (i_fcnvfxt(x)) => emit_cnv(3, x)

	      | (i_fcmp(FREG r1, FREG r2, fpf, cond)) =>
		    emit(0x0c, (r1 << 21) ++ (r2 << 16) ++ fpcond2b(cond) ++
			 (fpf2b(fpf) << 11) ++ (2 << 9))
	      | (i_ftest) =>
		    emit(0x0c, (1 << 13) ++ (2 << 9) ++ (1 << 5))

	      | (i_fadd(x)) => emit_fop(0, x)
	      | (i_fsub(x)) => emit_fop(1, x)
	      | (i_fmpy(x)) => emit_fop(2, x)
	      | (i_fdiv(x)) => emit_fop(3, x)

	      | (i_mpyadd(x)) => emit_fop2(0x06, x)
	      | (i_mpysub(x)) => emit_fop2(0x26, x)

	      | i_sdi_ldo(r1, le, r2) =>
		    emitInstr info (i_ldo(r1, eval_label_expr(info, loc)(le), r2))
	      | i_sdi_addil(le, r) =>
		    emitInstr info (i_addil(eval_label_expr(info, loc)(le), r))
	      | i_sdi_ldil(le, r) =>
		    emitInstr info (i_ldil(eval_label_expr(info, loc)(le), r))
	      | i_sdi_ldw(r1, le, r2) =>
		    emitInstr info (i_ldw(r1, eval_label_expr(info, loc)(le), r2))
	      | i_sdi_comb(r1, r2, cc c, le, n) =>
		    emitInstr info (i_combt(r1, r2, c, eval_label_expr(info, loc)(le), n))
	      | i_sdi_comb(r1, r2, cc_not c, le, n) =>
		    emitInstr info (i_combf(r1, r2, c, eval_label_expr(info, loc)(le), n))
	      | i_sdi_bl(le, n, r) =>
		    emitInstr info (i_bl(eval_label_expr(info, loc)(le), n, r))
	      | i_sdi_bb(r, im, c, le, n) =>
		    emitInstr info (i_bb(r, im, c, eval_label_expr(info, loc)(le), n))
	      | i_nullified(i) =>
		    emitInstr info i

	      |  i => ErrorMsg.impossible ("[HppaMCEmit.emitInstr "
					   ^ HppaAsHelp.instr2string(info)(i) ^ "]")

end (* local *)

    fun comment _ = ()

    fun init n = (code := ByteArray.array(n, 0);
		  loc := 0)

end (* structure HppaMCEmit *)
