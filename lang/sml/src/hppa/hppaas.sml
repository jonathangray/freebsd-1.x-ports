(* Copyright 1992 Scott Draves *)

structure HppaAsCode =
struct
    val outfile = ref std_out
end (* HppaAsCode *)

structure HppaAsHelp =
    struct

	open HppaInstr
	
	val new_line = "\n"
	val comma = ","
	val tab = "\t"

	fun int n =
	    if n >= 0 then
		Integer.makestring n
	    else
		"-" ^ Integer.makestring(~n)

	fun reg(REG r) = ("r" ^ makestring(r))
	fun creg(CREG r) = ("c" ^ makestring(r))
	fun freg(FREG r) = ("f" ^ makestring(r))
	fun sreg(SREG r) = ("s" ^ makestring(r))

	local

      fun ms2string ms_none         = ""
	| ms2string ms_shift        = ",s"
	| ms2string ms_modify       = ",m"
	| ms2string ms_shift_modify = ",sm"

      fun mab2string mab_none   = ""
	| mab2string mab_before = ",mb"
	| mab2string mab_after  = ",ma"

      fun mbe2string mbe_begin        = ",b"
	| mbe2string mbe_end          = ",e"
	| mbe2string mbe_begin_modify = ",b,m"
	| mbe2string mbe_end_modify   = ",e,m"

      fun nullify2string n_nullify    = ",n"
	| nullify2string n_no_nullify = ""

      fun arithcond2string cc_never                  = ""
	| arithcond2string cc_equal                  = ",="
	| arithcond2string cc_less                   = ",<"
	| arithcond2string cc_less_or_equal          = ",<="
	| arithcond2string cc_less_unsigned          = ",<<"
	| arithcond2string cc_less_or_equal_unsigned = ",<<="
	| arithcond2string cc_signed_overflow        = ",sv"
	| arithcond2string cc_odd                    = ",od"

      fun arithcond22string (cc x)                             = arithcond2string(x)
	| arithcond22string (cc_not cc_never)                  = ",tr"
	| arithcond22string (cc_not cc_equal)                  = ",<>"
	| arithcond22string (cc_not cc_less)                   = ",>="
	| arithcond22string (cc_not cc_less_or_equal)          = ",>"
	| arithcond22string (cc_not cc_less_unsigned)          = ",>>="
	| arithcond22string (cc_not cc_less_or_equal_unsigned) = ",>>"
	| arithcond22string (cc_not cc_signed_overflow)        = ",nsv"
	| arithcond22string (cc_not cc_odd)                    = ",ev"

      fun addcond2string (cc cc_less_unsigned)              = ",nuv"
	| addcond2string (cc cc_less_or_equal_unsigned)     = ",znv"
	| addcond2string (cc_not cc_less_unsigned)          = ",uv"
	| addcond2string (cc_not cc_less_or_equal_unsigned) = ",vnz"
	| addcond2string x                                  = arithcond22string(x)

      fun logicond2string lc_never                      = ""
	| logicond2string lc_all_zero                   = ",="
	| logicond2string lc_leftmost_one               = ",<"
	| logicond2string lc_leftmost_one_or_all_zero   = ",<="
	| logicond2string lc_rightmost_one              = ",od"
	| logicond2string lc_always                     = ",tr"
	| logicond2string lc_some_one                   = ",<>"
	| logicond2string lc_leftmost_zero              = ",>="
	| logicond2string lc_leftmost_zero_and_some_one = ",>"
	| logicond2string lc_rightmost_zero             = ",ev"

      fun unitcond2string uc_never               = ""
	| unitcond2string uc_some_byte_zero      = ",sbz"
	| unitcond2string uc_some_halfword_zero  = ",shz"
	| unitcond2string uc_some_digit_carry    = ",sdc"
	| unitcond2string uc_some_byte_carry     = ",sbc"
	| unitcond2string uc_some_halfword_carry = ",shc"

      fun unitcond22string (uc x)                          = unitcond2string(x)
	| unitcond22string (uc_not uc_never)               = ",tr"
	| unitcond22string (uc_not uc_some_byte_zero)      = ",nbz"
	| unitcond22string (uc_not uc_some_halfword_zero)  = ",nhz"
	| unitcond22string (uc_not uc_some_digit_carry)    = ",ndc"
	| unitcond22string (uc_not uc_some_byte_carry)     = ",nbc"
	| unitcond22string (uc_not uc_some_halfword_carry) = ",nhc"

      fun shiftcond2string sc_never         = ""
	| shiftcond2string sc_all_zero      = ",="
	| shiftcond2string sc_leftmost_one  = ",<"
	| shiftcond2string sc_rightmost_one = ",od"
	| shiftcond2string sc_always        = ",tr"
	| shiftcond2string sc_some_one      = ",<>"
	| shiftcond2string sc_leftmost_zero = ",>="
	| shiftcond2string sc_righmost_zero = ",ev"


      fun fpformat2string fpf_single = ",sgl"
	| fpformat2string fpf_double = ",dbl"
	| fpformat2string fpf_quad   = ",quad"

      fun fphalf FrLeftHalf  = "L"
	| fphalf FrRightHalf = "R"

      fun fpcond2string _ = ",XXX"

      fun ld(base, offset, target) =
	  (int(offset) ^ "(" ^ reg(base) ^ ")," ^ reg(target))

      fun st(base, offset, target) =
	  (reg(target) ^ comma ^ int(offset) ^ "(" ^ reg(base) ^ ")")

      fun ld_offset(base, offset, target) =
	  (tab ^ ld(base, offset, target))

      fun ld_index(base, index, ms, target) =
	  (ms2string(ms) ^ tab ^ reg(index) ^ "(" ^ reg(base) ^ ")," ^ reg(target))

      fun ld_short(base, offset, mab, target) =
	  (mab2string(mab) ^ tab ^ ld(base, offset, target))
	  
      fun st_offset(base, offset, target) =
	  (tab ^ st(base, offset, target))

      fun st_short(base, offset, mab, target) =
	  (mab2string(mab) ^ tab ^ st(base, offset, target))

      fun reg_reg_branch(r1, r2, cond, offset, nullify) =
	  (arithcond2string(cond) ^ nullify2string(nullify) ^ tab ^
	   reg(r1) ^ comma ^ reg(r2) ^ comma ^ int(offset))

      fun reg_im_branch(im, r2, cond, offset, nullify) =
	  (arithcond2string(cond) ^ nullify2string(nullify) ^ tab ^
	   int(im) ^ comma ^ reg(r2)^ comma ^ int(offset))

      fun reg_reg_reg(r1, r2, target) =
	  (tab ^ reg(r1) ^ comma ^ reg(r2) ^ comma ^ reg(target))
	   
      fun sub_reg_reg_reg(r1, r2, cond, target) =
	  (arithcond22string(cond) ^ reg_reg_reg(r1, r2, target))

      fun add_reg_reg_reg(r1, r2, cond, target) =
	  (addcond2string(cond) ^
	   reg_reg_reg(r1, r2, target))	   

      fun log_reg_reg_reg(r1, r2, cond, target) =
	  (logicond2string(cond) ^
	   reg_reg_reg(r1, r2, target))	   

      fun unit_reg_reg_reg(r1, r2, cond, target) =
	  (unitcond22string(cond) ^
	   reg_reg_reg(r1, r2, target))	   

      fun im_reg_reg(im, r2, target) =
	  (tab ^ int(im) ^ comma ^ reg(r2) ^ comma ^ reg(target))

      fun sub_im_reg_reg(im, r2, cond, target) =
	  (arithcond22string(cond) ^
	   im_reg_reg(im, r2, target))

      fun add_im_reg_reg(im, r2, cond, target) =
	  (addcond2string(cond) ^
	   im_reg_reg(im, r2, target))

      fun reg_im_reg(r1, cond, im, target) =
	  (shiftcond2string(cond) ^ tab ^
	   reg(r1) ^ comma ^ int(im) ^ comma ^ reg(target))
	   
      fun reg_im_im_reg(r1, cond, im1, im2, target) =
	  (shiftcond2string(cond) ^ tab ^
	   reg(r1) ^ comma ^ int(im1) ^ comma ^ int(im2) ^ comma ^ reg(target))
	   
      fun im_im_reg(im1, cond, im2, target) =
	  (shiftcond2string(cond) ^ tab ^
	   int(im1) ^ comma ^ int(im2) ^ comma ^ reg(target))

      fun im_im_im_reg(im1, cond, im2, im3, target) =
	  (shiftcond2string(cond) ^ tab ^
	   int(im1) ^ comma ^ int(im2) ^ comma ^ int(im3) ^ comma ^ reg(target))
	   
      fun fld_index(base, index, ms, target) =
	  (ms2string(ms) ^ tab ^
	   reg(index) ^ "(" ^ reg base ^ ")," ^ freg(target))

      fun fldw_index(base, index, ms, target,half) =
	  (ms2string(ms) ^ tab ^
	   reg(index) ^ "(" ^ reg base ^ ")," ^ freg(target) ^ fphalf half)

      fun fst_index(base, index, ms, target) =
	  (ms2string(ms) ^ tab ^
	   freg(target) ^ comma ^ reg(index) ^ "(" ^ reg(base) ^ ")")

      fun fstw_index(base, index, ms, target, half) =
	  (ms2string(ms) ^ tab ^  freg(target) ^ fphalf half ^ 
	   comma ^ reg(index) ^ "(" ^ reg(base) ^ ")")

      fun fld_short(base, index, mab, target) =
	  (mab2string(mab) ^ tab ^
	   int(index) ^ "(" ^ reg(base) ^ ")," ^ freg(target))

      fun fldw_short(base, index, mab, target, half) =
	  (mab2string(mab) ^ tab ^
	   int(index) ^ "(" ^ reg(base) ^ ")," ^ freg(target) ^ fphalf half)

      fun fst_short(base, index, mab, target) =
	  (mab2string(mab) ^ tab ^
	   freg(target) ^ comma ^ int(index) ^ "(" ^ reg(base) ^ ")")

      fun fstw_short(base, index, mab, target, half) =
	  (mab2string(mab) ^ tab ^
	   freg(target) ^ fphalf half ^ comma ^ int(index) ^ 
	   "(" ^ reg(base) ^ ")")

      fun fmt_freg_freg(r1, fmt, r2) =
	  (fpformat2string(fmt) ^ tab ^ freg(r1) ^ comma ^ freg(r2))

      fun fmt_freg_fmt_freg(r1, fmt1, r2, fmt2) =
	  (fpformat2string(fmt1) ^ fpformat2string(fmt2) ^ tab ^ freg(r1) ^ comma ^ freg(r2))

      fun freg_freg_freg(r1, r2, fmt, target) =
	  (fpformat2string(fmt) ^ tab ^ freg(r1) ^ comma ^ freg(r2) ^ comma ^ freg(target))

      fun five_freg(r1, r2, r3, r4, r5) =
	  (tab ^ freg(r1) ^ comma ^ freg(r2) ^ comma ^
	   freg(r3) ^ comma ^ freg(r4) ^ comma ^ freg(r5))


      fun le2string (INFO{nameOf, addrOf, ...}, (le_label lab)) =
	  (nameOf lab) ^ "=" ^ (makestring(addrOf lab))
	| le2string (info, le_hi21(le)) = "hi21(" ^ le2string(info, le) ^ ")"
	| le2string (info, le_lo11(le)) = "lo11(" ^ le2string(info, le) ^ ")"
	| le2string (info, le_negate(le)) = "~(" ^ le2string(info, le) ^ ")"
	| le2string (info, le_sum(le1, le2)) =
	  "(" ^ le2string(info, le1)^ "+" ^ le2string(info, le2) ^ ")"
	| le2string (info, le_diff(le1, le2)) =
	  "(" ^ le2string(info, le1)^ "-" ^ le2string(info, le2) ^ ")"
	| le2string (info, le_divn(le, n)) =
	  "(" ^ le2string(info, le) ^ "/" ^ (makestring n) ^ ")"
	| le2string (info, le_const(n)) = (makestring n)
	| le2string (info, le_dot) = "dot"


    in

	fun instr2string info I =
	    case I of
		i_nop    => "nop\t\t"
	      | i_ldw(x) => ("ldw" ^ ld_offset(x))
	      | i_ldh(x) => ("ldh" ^ ld_offset(x))
	      | i_ldb(x) => ("ldb" ^ ld_offset(x))
	      | i_stw(x) => ("stw" ^ st_offset(x))
	      | i_sth(x) => ("sth" ^ st_offset(x))
	      | i_stb(x) => ("stb" ^ st_offset(x))
	      | i_ldwm(x) => ("ldwm" ^ ld_offset(x))
	      | i_stwm(x) => ("stwm" ^ st_offset(x))

	      | i_ldwx(x) => ("ldwx" ^ ld_index(x))
	      | i_ldhx(x) => ("ldhx" ^ ld_index(x))
	      | i_ldbx(x) => ("ldbx" ^ ld_index(x))
	      | i_ldwax(x) => ("ldwax" ^ ld_index(x))
	      | i_ldcwx(x) => ("ldcwx" ^ ld_index(x))

	      | i_ldws(x) => ("ldws" ^ ld_short(x))
	      | i_ldhs(x) => ("ldhs" ^ ld_short(x))
	      | i_ldbs(x) => ("ldbs" ^ ld_short(x))
	      | i_ldwas(x) => ("ldwas" ^ ld_short(x))
	      | i_ldcws(x) => ("ldcws" ^ ld_short(x))
	      | i_stws(x) => ("stws" ^ ld_short(x))
	      | i_sths(x) => ("sths" ^ ld_short(x))
	      | i_stbs(x) => ("stbs" ^ ld_short(x))
	      | i_stwas(x) => ("stwas" ^ ld_short(x))

	   | i_stbys(base, offset, mbe, target) =>
		 ("stbys" ^ mbe2string(mbe) ^ tab ^ st(base, offset, target))

           | i_ldo(base, offset, target) =>
		 ("ldo" ^ tab ^ int(offset) ^ "(" ^ reg(base) ^ ")," ^ reg(target))

	   | i_ldil(offset, target) =>
		 ("ldil" ^ tab ^ int(offset) ^ comma ^ reg(target))
		 
	   | i_addil(offset, target) =>
		 ("addil" ^ tab ^ int(offset) ^ comma ^ reg(target))

	   | i_bl(offset, nullify, link) =>
		 ("bl" ^ nullify2string(nullify) ^ tab ^ int(offset) ^ comma ^ reg(link))

	   | i_gate(offset, nullify, link) =>
		 ("gate" ^ nullify2string(nullify) ^ tab ^ int(offset) ^ comma ^ reg(link))

	   | i_blr(offset, nullify, link) =>
		 ("blr" ^ nullify2string(nullify) ^ reg(offset) ^ comma ^ reg(link))
		 
	   | i_bv(offset, nullify, base) =>
		 ("bv" ^ nullify2string(nullify) ^ tab ^ reg(offset) ^ "(" ^ reg(base) ^ ")")

	   | i_be(offset, nullify, space, base) =>
		 ("be" ^  nullify2string(nullify) ^ tab ^ int(offset) ^
		  "(" ^ sreg(space) ^ comma ^ reg(base) ^ ")")

	   | i_ble(offset, nullify, space, base) =>
		 ("ble" ^  nullify2string(nullify) ^ tab ^ int(offset) ^
		  "(" ^ sreg(space) ^ comma ^ reg(base) ^ ")")

	   | i_movb(x) => ("movb" ^ reg_reg_branch(x))
	   | i_movib(x) => ("movib" ^ reg_im_branch(x))
	   | i_combt(x) => ("combt" ^ reg_reg_branch(x))
	   | i_combf(x) => ("combf" ^ reg_reg_branch(x))
	   | i_comibt(x) => ("comibt" ^ reg_im_branch(x))
	   | i_comibf(x) => ("comibf" ^ reg_im_branch(x))
	   | i_addbt(x) => ("addbt" ^ reg_reg_branch(x))
	   | i_addbf(x) => ("addbf" ^ reg_reg_branch(x))
	   | i_addibt(x) => ("addibt" ^ reg_im_branch(x))
	   | i_addibf(x) => ("addibf" ^ reg_im_branch(x))

	   | i_bvb(r, shiftcond, offset, nullify) =>
		 ("bvb" ^ shiftcond2string(shiftcond) ^ nullify2string(nullify) ^ tab ^
		  reg(r) ^ comma ^ int(offset))

	   | i_bb(r, im, shiftcond, offset, nullify) =>
		 ("bb" ^ shiftcond2string(shiftcond) ^ nullify2string(nullify) ^ tab ^
		  reg(r) ^ comma ^ int(im) ^ comma ^ int(offset))

	   | i_add(x)     => ("add" ^     add_reg_reg_reg(x))
	   | i_addl(x)    => ("addl" ^    add_reg_reg_reg(x))
	   | i_addo(x)    => ("addo" ^    add_reg_reg_reg(x))
	   | i_addc(x)    => ("addc" ^    add_reg_reg_reg(x))
	   | i_addco(x)   => ("addco" ^   add_reg_reg_reg(x))
	   | i_sh1add(x)  => ("sh1add" ^  add_reg_reg_reg(x))
	   | i_sh1addl(x) => ("sh1addl" ^ add_reg_reg_reg(x))
	   | i_sh1addo(x) => ("sh1addo" ^ add_reg_reg_reg(x))
	   | i_sh2add(x)  => ("sh2add" ^  add_reg_reg_reg(x))
	   | i_sh2addl(x) => ("sh2addl" ^ add_reg_reg_reg(x))
	   | i_sh2addo(x) => ("sh2addo" ^ add_reg_reg_reg(x))
	   | i_sh3add(x)  => ("sh3add" ^  add_reg_reg_reg(x))
	   | i_sh3addl(x) => ("sh3addl" ^ add_reg_reg_reg(x))
	   | i_sh3addo(x) => ("sh3addo" ^ add_reg_reg_reg(x))
	   | i_sub(x)    => ("sub" ^    sub_reg_reg_reg(x))
	   | i_subo(x)   => ("subo" ^   sub_reg_reg_reg(x))
	   | i_subb(x)   => ("subb" ^   sub_reg_reg_reg(x))
	   | i_subbo(x)  => ("subbo" ^  sub_reg_reg_reg(x))
	   | i_subt(x)   => ("subt" ^   sub_reg_reg_reg(x))
	   | i_subto(x)  => ("subto" ^  sub_reg_reg_reg(x))
	   | i_ds(x)     => ("ds" ^     sub_reg_reg_reg(x))
	   | i_comclr(x) => ("comclr" ^ sub_reg_reg_reg(x))
	   | i_or(x)     => ("or" ^    log_reg_reg_reg(x))
	   | i_xor(x)    => ("xor" ^   log_reg_reg_reg(x))
	   | i_and(x)    => ("and" ^   log_reg_reg_reg(x))
	   | i_andcm(x)  => ("andcm" ^ log_reg_reg_reg(x))
	   | i_uxor(x)   => ("uxor" ^    unit_reg_reg_reg(x))
	   | i_uaddcm(x) => ("uaddcm" ^  unit_reg_reg_reg(x))
	   | i_uaddcmt(x)=> ("uaddcmt" ^ unit_reg_reg_reg(x))
	   | i_dcor(r1, r2, cond) =>
		 ("dcor" ^ unitcond22string(cond) ^ tab ^ reg(r1) ^ comma ^ reg(r2))
	   | i_idcor(r1, r2, cond) =>
		 ("idcor" ^ unitcond22string(cond) ^ tab ^ reg(r1) ^ comma ^ reg(r2))
	   | i_addi(x)      => ("addi" ^    add_im_reg_reg(x))
	   | i_addio(x)     => ("addio" ^   add_im_reg_reg(x))
	   | i_addit(x)     => ("addit" ^   add_im_reg_reg(x))
	   | i_addito(x)    => ("addito" ^  add_im_reg_reg(x))
	   | i_subi(x)      => ("subi" ^    sub_im_reg_reg(x))
	   | i_subio(x)     => ("subio" ^   sub_im_reg_reg(x))
	   | i_comiclr(x)   => ("comiclr" ^ sub_im_reg_reg(x))
	   | i_vshd(r1, r2, cond, target) =>
		 ("vshd" ^ shiftcond2string(cond) ^ tab ^
		  reg(r1) ^ comma ^ reg(r2) ^ comma ^ reg(target))
	   | i_shd(r1, r2, cond, by, target) =>
		 ("shd" ^ shiftcond2string(cond) ^ tab ^
		  reg(r1) ^ comma ^ reg(r2) ^ comma ^ int(by) ^ comma ^ reg(target))
	   | i_vextru(x)   => ("vextru" ^  reg_im_reg(x))
	   | i_vextrs(x)   => ("vextrs" ^  reg_im_reg(x))
	   | i_extru(x)    => ("extru" ^   reg_im_im_reg(x))
	   | i_extrs(x)    => ("extrs" ^   reg_im_im_reg(x))
	   | i_vdep(x)     => ("vdep" ^    reg_im_reg(x))
	   | i_dep(x)      => ("dep" ^     reg_im_im_reg(x))
	   | i_vdepi(x)    => ("vdepi" ^   im_im_reg(x))
	   | i_depi(x)     => ("depi" ^    im_im_im_reg(x))
	   | i_zvdep(x)    => ("zvdep" ^   reg_im_reg(x))
	   | i_zdep(x)     => ("zdep" ^    reg_im_im_reg(x))
	   | i_zvdepi(x)   => ("zvdepi" ^  im_im_reg(x))
	   | i_zdepi(x)    => ("zdepi" ^   im_im_im_reg(x))
	   | i_break(im1, im2) => ("break" ^ tab ^ int(im1) ^ comma ^ int(im2))
	   | i_rfi => "rfi"
	   | i_rfir => "rfir"
	   | i_ssm(i, r) => ("ssm" ^ tab ^ int(i) ^ comma ^ reg(r))
	   | i_rsm(i, r) => ("rsm" ^ tab ^ int(i) ^ comma ^ reg(r))
	   | i_mtsm(r) => ("mtsm" ^ tab ^ reg(r))
	   | i_mtctl(r, c) => ("mtctl" ^ tab ^ reg(r) ^ comma ^ creg(c))
	   | i_mfctl(c, r) => ("mfctl" ^ tab ^ creg(c) ^ comma ^ reg(r))
	   | i_ldsid(r, t) => ("ldsid" ^ tab ^ "(" ^ reg(r) ^ ")" ^ reg(t))
	   | i_mtsp(r, s) => ("mtsp" ^ tab ^ reg(r) ^ comma ^ sreg(s))
	   | i_mfsp(s, r) => ("mfsp" ^ tab ^ sreg(s) ^ comma ^ reg(r))

	   | i_sync => "sync"

	   | i_fldwx(x) => ("fldwx" ^ fldw_index(x))
	   | i_flddx(x) => ("flddx" ^ fld_index(x))
	   | i_fstwx(x) => ("fstwx" ^ fstw_index(x))
	   | i_fstdx(x) => ("fstdx" ^ fst_index(x))
		 
	   | i_fldws(x) => ("fldws" ^ fldw_short(x))
	   | i_fldds(x) => ("fldds" ^ fld_short(x))
	   | i_fstws(x) => ("fstws" ^ fstw_short(x))
	   | i_fstds(x) => ("fstds" ^ fst_short(x))

	   | i_fcpy(x)  => ("fcpy" ^  fmt_freg_freg(x))
	   | i_fabs(x)  => ("fabs" ^  fmt_freg_freg(x))
	   | i_fsqrt(x) => ("fsqrt" ^ fmt_freg_freg(x))
	   | i_frnd(x)  => ("frnd" ^  fmt_freg_freg(x))

	   | i_fcnvff(x)  => ("fcnvff" ^  fmt_freg_fmt_freg(x))
	   | i_fcnvxf(x)  => ("fcnvxf" ^  fmt_freg_fmt_freg(x))
	   | i_fcnvfx(x)  => ("fcnvfx" ^  fmt_freg_fmt_freg(x))
	   | i_fcnvfxt(x) => ("fcnvfxt" ^ fmt_freg_fmt_freg(x))

	   | i_fcmp(f1, f2, fmt, cond) =>
		 ("fcmp" ^ fpformat2string(fmt) ^ fpcond2string(cond) ^ tab ^
		  freg(f1) ^ freg(f2))

	   | i_ftest =>"ftest"

	   | i_fadd(x) => ("fadd" ^ freg_freg_freg(x))
	   | i_fsub(x) => ("fsub" ^ freg_freg_freg(x))
	   | i_fmpy(x) => ("fmpy" ^ freg_freg_freg(x))
	   | i_fdiv(x) => ("fdiv" ^ freg_freg_freg(x))

	   | i_xmpyu(r1, r2, target) =>
		 ("xmpyu" ^ tab ^
		  freg(r1) ^ comma ^ freg(r2) ^ comma ^ freg(target))
		 
	   | i_mpyadd(x) => ("mpyadd" ^ five_freg(x))
	   | i_mpysub(x) => ("mpysub" ^ five_freg(x))

	   | i_sdi_ldo(base, le, target) =>
		 ("*ldo" ^ tab ^ le2string(info, le) ^ "(" ^ reg(base) ^ ")," ^ reg(target))
	   | i_sdi_addil(le, target) =>
		 ("*addil" ^ tab ^ le2string(info, le) ^ comma ^ reg(target))
	   | i_sdi_ldil(le, target) =>
		 ("*ldil" ^ tab ^ le2string(info, le) ^ comma ^ reg(target))
	   | i_sdi_ldw(base, le, target) =>
		 ("*ldw" ^ tab ^ le2string(info, le) ^ "(" ^ reg(base) ^ ")," ^ reg(target))
	   | i_sdi_comb(r1, r2, cc cond, le, nullify) =>
		 ("*combt" ^ arithcond2string(cond) ^ nullify2string(nullify) ^ tab ^
		  reg(r1) ^ comma ^ reg(r2) ^ comma ^ le2string(info, le))
	   | i_sdi_comb(r1, r2, cc_not cond, le, nullify) =>
		 ("*combf" ^ arithcond2string(cond) ^ nullify2string(nullify) ^ tab ^
		  reg(r1) ^ comma ^ reg(r2) ^ comma ^ le2string(info, le))
	   | i_sdi_bl(le, nullify, link) =>
		 ("*bl" ^ nullify2string(nullify) ^ tab ^ le2string(info, le) ^ comma ^ reg(link))
	   | i_sdi_bb(r, im, shiftcond, le, nullify) =>
		 ("*bb" ^ shiftcond2string(shiftcond) ^ nullify2string(nullify) ^ tab ^
		  reg(r) ^ comma ^ int(im) ^ comma ^ le2string(info, le))

	   | i_nullified(i) =>
		 "nullified " ^ (instr2string info i)

	  (* end of case *)

    end (* local *)
end (* struct HppaAsHelp *)

structure HppaAsEmit : EMITTER =
struct

    open HppaAsCode HppaAsHelp

  (** The location counter **)
    val loc = ref 0
    fun advance n = (loc := !loc + n)
    fun advance4 () = (loc := !loc + 4)
    fun emit s = output (!outfile, s)
    fun emit_new_line () = emit new_line

    fun emitLabel (INFO{nameOf, ...}) lab = emit(nameOf lab)

    fun emitOffset 0 = ()
      | emitOffset i = (if (i < 0)
	  then (emit "-"; emit(int(~i)))
	  else (emit "+"; emit(int i)))

    fun emitLong i = (emit "\t.long\t";
		      emit(int i);
		      emit_new_line();
		      advance4())

    (* why isn't stuff like this shared? XXX *)
    local
	fun oct i = let
			val m = Integer.makestring
		    in
			m(i quot 64) ^
			m((i quot 8)mod 8) ^
			m(i mod 8)
		    end
	fun c_char "\n" = "\\n"
	  | c_char "\t" = "\\t"
	  | c_char "\\" = "\\\\"
	  | c_char "\"" = "\\\""
	  | c_char c = if ord c < 32 then "\\"^oct(ord c) else c
	fun a_str s = implode(map c_char (explode s))
    in
	fun emitString s =
	    (emit "\t.ascii \"";
	     emit(a_str s);
	     emit "\"\n";
	     emit "\t.align\t4\n";
	     advance(size s))
    end (* local *)

    exception BadReal of string
    fun emitReal r =
	(emit "\t.double\t";
	 emit r;
	 emit_new_line();
	 advance 8)

    fun emitAddr (info as INFO{addrOf, ...}) (lab, k) =
	(emit "\t.long\t(";
	 emitLabel info lab;
	 emit "-.)";
	 emitOffset k;
	 emit "\t| ";
	 emit(int(k + addrOf(lab) - !loc));
	 emit_new_line();
	 advance4())

    fun define (info as INFO{addrOf, ...}) lab =
	(emitLabel info lab;
	 emit ":\t\t\t\t| loc = ";
	 emit(int(!loc));
	 emit ", addr = ";
	 emit(int(addrOf(lab)));
	 emit_new_line())

    local
	open System.Tags
    in
	fun mark () = (emit "\t.long\t";
		       emit (int (make_desc((!loc + 4) quot 4, tag_backptr)));
		       emit_new_line();
		       advance4())
    end


    fun emitInstr info I =
	(emit (tab ^ (instr2string info I) ^ "\t| loc = " ^ int(!loc) ^ new_line);
	 advance4())

    fun comment s = emit s

    fun init (n : int) = (
	  loc := 0;
	  emit "| code size = "; emit(makestring n); emit " bytes\n")

end (* structure HppaAsEmit *)
