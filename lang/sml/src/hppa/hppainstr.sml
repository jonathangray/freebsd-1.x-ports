(* Copyright 1992 Scott Draves *)

structure HppaInstrSet =
struct

    datatype register  =  REG of int  (* general purpose *)
    datatype fregister = FREG of int  (* floating point *)
    datatype cregister = CREG of int  (* control *)
    datatype sregister = SREG of int  (* space (aka segment) *)

    val zero_reg  =  REG 0   (* wired to 0 *)
    val addr_reg  =  REG 1   (* used by addil *)
    val shift_reg = CREG 11  (* used by variable shift/bitfield *)

    type im5  = int
    type im11 = int
    type im12 = int
    type im13 = int
    type im14 = int
    type im17 = int
    type im21 = int

    fun im5  n =  n <    16 andalso n >=    ~16
    fun im11 n =  n <  1024 andalso n >=  ~1024
    fun im12 n =  n <  2048 andalso n >=  ~2048
    fun im14 n =  n <  8192 andalso n >=  ~8192
    fun im17 n =  n < 65536 andalso n >= ~65536

    fun hi21 n = Bits.rshift(n, 11)
    fun lo11 n = Bits.andb(n, 0x7ff)

    datatype modifyshift
	= ms_none | ms_shift | ms_modify | ms_shift_modify

    datatype modifyab
	= mab_none | mab_before | mab_after

    datatype modifybe
	= mbe_begin | mbe_end | mbe_begin_modify | mbe_end_modify

    datatype nullify
	= n_nullify | n_no_nullify

    datatype arithcond
      = cc_never
      | cc_equal
      | cc_less
      | cc_less_or_equal
      | cc_less_unsigned
      | cc_less_or_equal_unsigned
      | cc_signed_overflow
      | cc_odd

    datatype arithcond2 = cc of arithcond | cc_not of arithcond

    val never = cc cc_never;

    datatype addcond
	= ac_never

    datatype logicond
	= lc_never
        | lc_all_zero
	| lc_leftmost_one
	| lc_leftmost_one_or_all_zero
	| lc_rightmost_one
	| lc_always
	| lc_some_one
	| lc_leftmost_zero
	| lc_leftmost_zero_and_some_one
	| lc_rightmost_zero

    datatype unitcond
	= uc_never
        | uc_some_byte_zero
        | uc_some_halfword_zero
        | uc_some_digit_carry
        | uc_some_byte_carry
        | uc_some_halfword_carry

    datatype unitcond2 = uc of unitcond | uc_not of unitcond

    datatype shiftcond
	= sc_never
	| sc_all_zero
	| sc_leftmost_one
	| sc_rightmost_one
	| sc_always
	| sc_some_one
	| sc_leftmost_zero
	| sc_rightmost_zero

    datatype fpformat
	= fpf_single | fpf_double | fpf_quad

    datatype fpcond
	= fpc of {less      : bool,
		  greater   : bool,
		  equal     : bool,
		  unordered : bool,
		  trap      : bool
		  }

    datatype fphalf = FrLeftHalf | FrRightHalf

    (* we ignore most spaces, cache hints, and TLB operations.
       the fields are in the same order as they appear in the
       assembly code, with the exception of the store instructions,
       where the src and dst are reversed (this makes sense if you
       think about it long enough) *)


    datatype 'label label_expr
	= le_label   of 'label
        | le_hi21    of 'label label_expr
        | le_lo11    of 'label label_expr
        | le_negate  of 'label label_expr
        | le_sum     of 'label label_expr * 'label label_expr
        | le_diff    of 'label label_expr * 'label label_expr
        | le_divn    of 'label label_expr * int
        | le_const   of int
	| le_dot     (* the location of the instruction containing the expr *)

	
    datatype 'label instruction

	= i_nop (* synthetic *)

	(* memory instructions *)
	            (* base     offset   target/source *)
	| i_ldw     of register * im14 * register
	| i_ldh     of register * im14 * register
	| i_ldb     of register * im14 * register
	| i_stw     of register * im14 * register
	| i_sth     of register * im14 * register
	| i_stb     of register * im14 * register
        | i_ldwm    of register * im14 * register
	| i_stwm    of register * im14 * register

	            (* base       index                    target *)
	| i_ldwx    of register * register * modifyshift * register
	| i_ldhx    of register * register * modifyshift * register
	| i_ldbx    of register * register * modifyshift * register
	| i_ldwax   of register * register * modifyshift * register
	| i_ldcwx   of register * register * modifyshift * register

	            (* base    offset              target *)
	| i_ldws    of register * im5 * modifyab * register
	| i_ldhs    of register * im5 * modifyab * register
	| i_ldbs    of register * im5 * modifyab * register
	| i_ldwas   of register * im5 * modifyab * register
	| i_ldcws   of register * im5 * modifyab * register
	| i_stws    of register * im5 * modifyab * register
	| i_sths    of register * im5 * modifyab * register
	| i_stbs    of register * im5 * modifyab * register
	| i_stwas   of register * im5 * modifyab * register

	| i_stbys   of register * im5 * modifybe * register

	(* immediate instructions *)
	| i_ldo     of register * im14 * register
	| i_ldil    of im21 * register
	| i_addil   of im21 * register

	(* branches *)
	| i_bl      of im17 * nullify * register
	| i_gate    of im17 * nullify * register
	| i_blr     of register * nullify * register
	| i_bv      of register * nullify * register
	| i_be      of im17 * nullify * sregister * register
	| i_ble     of im17 * nullify * sregister * register
	| i_movb    of register * register * arithcond * im12 * nullify
	| i_movib   of im5 * register * arithcond * im12 * nullify
	| i_combt   of register * register * arithcond * im12 * nullify
	| i_combf   of register * register * arithcond * im12 * nullify
	| i_comibt  of im5 * register * arithcond * im12 * nullify
	| i_comibf  of im5 * register * arithcond * im12 * nullify
	| i_addbt   of register * register * arithcond * im12 * nullify
	| i_addbf   of register * register * arithcond * im12 * nullify
	| i_addibt  of im5 * register * arithcond * im12 * nullify
	| i_addibf  of im5 * register * arithcond * im12 * nullify
	| i_bvb     of register * shiftcond * im12 * nullify
	| i_bb      of register * im5 * shiftcond * im12 * nullify

	(* computation *)
	| i_add     of register * register * arithcond2 * register
	| i_addl    of register * register * arithcond2 * register
	| i_addo    of register * register * arithcond2 * register
	| i_addc    of register * register * arithcond2 * register
	| i_addco   of register * register * arithcond2 * register
	| i_sh1add  of register * register * arithcond2 * register
	| i_sh1addl of register * register * arithcond2 * register
	| i_sh1addo of register * register * arithcond2 * register
	| i_sh2add  of register * register * arithcond2 * register
	| i_sh2addl of register * register * arithcond2 * register
	| i_sh2addo of register * register * arithcond2 * register
	| i_sh3add  of register * register * arithcond2 * register
	| i_sh3addl of register * register * arithcond2 * register
	| i_sh3addo of register * register * arithcond2 * register
	| i_sub     of register * register * arithcond2 * register
	| i_subo    of register * register * arithcond2 * register
	| i_subb    of register * register * arithcond2 * register
	| i_subbo   of register * register * arithcond2 * register
	| i_subt    of register * register * arithcond2 * register
	| i_subto   of register * register * arithcond2 * register
	| i_ds      of register * register * arithcond2 * register
	| i_comclr  of register * register * arithcond2 * register
	| i_or      of register * register * logicond * register
	| i_xor     of register * register * logicond * register
	| i_and     of register * register * logicond * register
	| i_andcm   of register * register * logicond * register
	| i_uxor    of register * register * unitcond2 * register
	| i_uaddcm  of register * register * unitcond2 * register
	| i_uaddcmt of register * register * unitcond2 * register
	| i_dcor    of register * register * unitcond2
	| i_idcor   of register * register * unitcond2
	| i_addi    of im11 * register * arithcond2 * register
	| i_addio   of im11 * register * arithcond2 * register
	| i_addit   of im11 * register * arithcond2 * register
	| i_addito  of im11 * register * arithcond2 * register
	| i_subi    of im11 * register * arithcond2 * register
	| i_subio   of im11 * register * arithcond2 * register
	| i_comiclr of im11 * register * arithcond2 * register
	| i_vshd    of register * register * shiftcond * register
	            (* hibits     lobits             shiftby   target *)
	| i_shd     of register * register * shiftcond * im5 * register
	| i_vextru  of register * shiftcond * im5 * register
	| i_vextrs  of register * shiftcond * im5 * register
	            (* source                 start len   target *)
	| i_extru   of register * shiftcond * im5 * im5 * register
	| i_extrs   of register * shiftcond * im5 * im5 * register
	| i_vdep    of register * shiftcond * im5 * register
	| i_dep     of register * shiftcond * im5 * im5 * register
	| i_vdepi   of im5 * shiftcond * im5 * register
	| i_depi    of im5 * shiftcond * im5 * im5 * register
	| i_zvdep   of register * shiftcond * im5 * register
	| i_zdep    of register * shiftcond * im5 * im5 * register
	| i_zvdepi  of im5 * shiftcond * im5 * register
	| i_zdepi   of im5 * shiftcond * im5 * im5 * register

        (* control instructions *)
	| i_break   of im5 * im13
	| i_rfi
	| i_rfir
	| i_ssm     of im5 * register
	| i_rsm     of im5 * register
	| i_mtsm    of register
	| i_mtctl   of register * cregister
	| i_mfctl   of cregister * register
	| i_ldsid   of register * register
	| i_mtsp    of register * sregister
	| i_mfsp    of sregister * register
	| i_sync

	(* floating point *)
	            (* base       index                    target *)
	| i_fldwx   of register * register * modifyshift * fregister * fphalf
	| i_fstwx   of register * register * modifyshift * fregister * fphalf
	| i_flddx   of register * register * modifyshift * fregister
	| i_fstdx   of register * register * modifyshift * fregister

	            (* base     index              target *)
	| i_fldws   of register * im5 * modifyab * fregister * fphalf
	| i_fstws   of register * im5 * modifyab * fregister * fphalf
	| i_fldds   of register * im5 * modifyab * fregister
	| i_fstds   of register * im5 * modifyab * fregister

	| i_fcpy    of fregister * fpformat * fregister
	| i_fabs    of fregister * fpformat * fregister
	| i_fsqrt   of fregister * fpformat * fregister
	| i_frnd    of fregister * fpformat * fregister

	| i_fcnvff  of fregister * fpformat * fregister * fpformat
	| i_fcnvxf  of fregister * fpformat * fregister * fpformat
	| i_fcnvfx  of fregister * fpformat * fregister * fpformat
	| i_fcnvfxt of fregister * fpformat * fregister * fpformat

	| i_fcmp    of fregister * fregister * fpformat * fpcond
	| i_ftest
	| i_fadd    of fregister * fregister * fpformat * fregister
	| i_fsub    of fregister * fregister * fpformat * fregister
	| i_fmpy    of fregister * fregister * fpformat * fregister
	| i_fdiv    of fregister * fregister * fpformat * fregister
	| i_xmpyu   of fregister * fregister * fregister
	| i_mpyadd  of fregister * fregister * fregister * fregister * fregister
	| i_mpysub  of fregister * fregister * fregister * fregister * fregister

        (* these instructions have fields whose values depends on the final
           positioning of the labels. they are the instructions generated by 
           the EXPAND fuction. *)
	| i_sdi_ldo    of register * 'label label_expr * register
	| i_sdi_addil  of 'label label_expr * register
	| i_sdi_ldil   of 'label label_expr * register
        | i_sdi_ldw    of register * 'label label_expr * register
	| i_sdi_comb   of register * register * arithcond2 * 'label label_expr * nullify
        | i_sdi_bl     of 'label label_expr * nullify * register
        | i_sdi_bb     of register * im5 * shiftcond * 'label label_expr * nullify

	  (* these instructions are ones that might be nullified by the
	     previous instruction. *)
	| i_nullified  of 'label instruction

    fun invert_cond (cc c) = (cc_not c)
      | invert_cond (cc_not c) = (cc c)

    (* XXX *)
    fun latency _ = 1

    (* 32 regs + 32 fp regs + 32 control regs + memory + npc *)
    val numResources = 98

    local
	fun reg r = r
	fun freg r = 32 + r
	fun creg r = 64 + r
	val mem = 96
	val npc = 97

	fun load(r1, r2) = ([mem, reg r1], [reg r2])
	fun store(r1, r2) = ([reg r1], [mem, reg r2])
    in

	(* this should sequentialize everything *)
	fun rUseDef x = ([0], [0])
	    (*
	    case x of
		i_nop => ([], [])
	      | i_ldw(r1, _, r2) => load(r1, r2)
	      | i_ldh(r1, _, r2) => load(r1, r2)
	      | i_ldb(r1, _, r2) => load(r1, r2)
	      | i_stw(r1, _, r2) => store(r1, r2)
	      | i_sth(r1, _, r2) => store(r1, r2)
	      | i_stb(r1, _, r2) => store(r1, r2)
	      | i_ldwm
		*)
    end

end (* HppaInstrSet *)
