(* Copyright 1992 Scott Draves *)

structure HppaInstr =
    struct
	open HppaInstrSet

	val error = ErrorMsg.impossible
	val branchDelayedArch = true
	val nop = i_nop

	datatype 'label info = INFO of {addrOf: 'label -> int, 
					nameOf: 'label -> string}

	datatype 'label EA =
	    Direct of register
	  | FDirect of fregister
	  | Immed of int
	  | ImmedLab of 'label

	datatype ikind = IK_NOP | IK_JUMP | IK_INSTR

	fun instrKind i_nop        = IK_NOP
	  | instrKind (i_bl     _) = IK_JUMP
	  | instrKind (i_gate   _) = IK_JUMP
	  | instrKind (i_blr    _) = IK_JUMP
	  | instrKind (i_bv     _) = IK_JUMP
	  | instrKind (i_movb   _) = IK_JUMP
	  | instrKind (i_movib  _) = IK_JUMP
	  | instrKind (i_combt  _) = IK_JUMP
	  | instrKind (i_combf  _) = IK_JUMP
	  | instrKind (i_comibt _) = IK_JUMP
	  | instrKind (i_comibf _) = IK_JUMP
	  | instrKind (i_addbt  _) = IK_JUMP
	  | instrKind (i_addbf  _) = IK_JUMP
	  | instrKind (i_addibt _) = IK_JUMP
	  | instrKind (i_addibf _) = IK_JUMP
	  | instrKind (i_bvb    _) = IK_JUMP
	  | instrKind (i_bb     _) = IK_JUMP
	  | instrKind (i_sdi_bl _) = IK_JUMP
	  | instrKind (i_sdi_bb _) = IK_JUMP
	  | instrKind (i_sdi_comb _) = IK_JUMP
	  | instrKind (i_nullified i) = instrKind i
	  | instrKind _            = IK_INSTR


	fun mayNeedNop i = if IK_JUMP = instrKind(i) then 1 else 0
	fun needsNop _ = 0


	val base_reg = REG 24
	(* XXX val base_reg_bias = 8192 - 4 *)

	datatype 'label sdi =
	    (* base_reg = register - label *)
	    set_base_reg of 'label * register

	  (* register = label + int *)
	  | load_addr of 'label * int * register

	  (* register = mem[label + int] *)
	  | load  of 'label * int * register

	  (* fregister = mem[lable + int] and a temp *)
	  | loadf of 'label * fregister * register 

	  (* if arithcond2(register1, register2) then jump *)
	  | cbranch of 'label * 'label * arithcond2 * register * register

	  (* if and(1<<int, register) then jump *)
	  | bbranch of 'label * 'label * int * register

	  (* if fpcond(fregister1, fregister2) then jump *)
	  | fbranch of 'label * register

	  (* register1 = pc; pc +=  label1 - label2 *)
	  | jump of 'label * register

	  (* pc +=  label1 - label2 + register1, register2 is temp,
	     not used *)
	  | jumpr of 'label * 'label * register * register


	fun isSdiBranch (cbranch _) = true
	  | isSdiBranch (fbranch _) = true
	  | isSdiBranch (bbranch _) = true
	  | isSdiBranch (jumpr _)   = true
	  | isSdiBranch (jump _)    = true
	  | isSdiBranch _           = false

	fun minSize (set_base_reg _) = 4
	  | minSize (load_addr _)    = 4
	  | minSize (load _)         = 4
	  | minSize (loadf _)        = 20
	  | minSize (cbranch _)      = 4
	  | minSize (bbranch _)      = 4
	  | minSize (fbranch _)      = 4
	  | minSize (jump _)         = 4
	  | minSize (jumpr _)        = 12

	fun offset_estimate (INFO{addrOf,...}, loc) lab =
	    ((addrOf lab) - loc - 8) div 4

	fun make_jump_offset_expr (lab) =
	    le_divn(le_sum(le_diff(le_label(lab), le_dot), le_const(~8)), 4)

	fun eval_label_expr (INFO{addrOf, ...}, _) (le_label lab) = addrOf(lab)
	  | eval_label_expr info (le_hi21(le)) = hi21(eval_label_expr(info)(le))
	  | eval_label_expr info (le_lo11(le)) = lo11(eval_label_expr(info)(le))
	  | eval_label_expr info (le_negate(le)) = ~(eval_label_expr(info)(le))
	  | eval_label_expr info (le_sum(le1, le2)) =
	    (eval_label_expr(info)(le1)) +  (eval_label_expr(info)(le2))
	  | eval_label_expr info (le_diff(le1, le2)) =
	    (eval_label_expr(info)(le1)) -  (eval_label_expr(info)(le2))
	  | eval_label_expr info (le_divn(le, n)) = (eval_label_expr(info)(le)) div n
	  | eval_label_expr info (le_const(n)) = n
	  | eval_label_expr (_, loc) le_dot = (!loc)



	fun sizeOf (INFO{addrOf,...}) (set_base_reg(lab, _), _) =
	    if im14(~(addrOf lab)) then (false, 4) else (true, 8)

	  | sizeOf (INFO{addrOf,...}) (load_addr(lab, k, _), _) =
	    if im14((addrOf lab) + k) then (false, 4) else (true, 8)

	  | sizeOf (INFO{addrOf,...}) (load(lab, k, _), _) =
	    if im14((addrOf lab) + k) then (false, 4) else (true, 8)

	  | sizeOf (INFO{addrOf,...}) (loadf(lab,k,_), _) = (true,20)

	  | sizeOf info (cbranch(dest_lab, skip_lab, _, _, _), loc) =
	    let val offset = offset_estimate(info, loc)(dest_lab)
	    in if im12(offset) then (false, 4)
	       else if im17(offset) then (false, 8)
		    else (true, 16)
	    end

	  | sizeOf info (fbranch(destLab,tmpR), loc) = let
	      val offset = offset_estimate(info,loc) destLab
            in
		if im17 offset then (false,4) else (true,12)
            end

	  | sizeOf info (bbranch(dest_lab, skip_lab, _, _), loc) = let
	        val offset = offset_estimate(info, loc)(dest_lab)
	    in 
		if im12(offset) then (false, 4) else (true,8)
	    end

	  | sizeOf info  (jump(dest_lab, _), loc) = let
	       val offset = offset_estimate(info, loc)(dest_lab)
	    in 
		if im17(offset) then (false, 4) else (true, 12)
	    end

	  | sizeOf info (jumpr(dest_lab, from_lab, _, _), _) =
	    (true, 12)

	fun expand info (set_base_reg(lab, reg), size, _) =
	    let val offset = le_negate(le_label lab)
	    in case size of
		4 => [i_sdi_ldo(reg, offset, base_reg)]
	      | 8 => [i_sdi_addil(le_hi21(offset), reg),
		      i_sdi_ldo(addr_reg, le_lo11(offset), base_reg)]
	      | _ => error "[HppaInstr.expand set_base_reg]"
	    end
	    
	  | expand info (load_addr(lab, k, reg), size, _) =
	    let val offset = le_sum(le_label(lab), le_const(k))
	    in case size of
		4 => [i_sdi_ldo(base_reg, offset, reg)]
	      | 8 => [i_sdi_addil(le_hi21(offset), base_reg),
		      i_sdi_ldo(addr_reg, le_lo11(offset), reg)]
	      | _ => error "[HppaInstr.expand load_addr]"
	    end

	  | expand info (load(lab, k, reg), size, _) =
	    let val offset = le_sum(le_label(lab), le_const(k))
	    in case size of
		4 => [i_sdi_ldw(base_reg, offset, reg)]
	      | 8 => [i_sdi_addil(le_hi21(offset), base_reg),
		      i_sdi_ldw(addr_reg, le_lo11(offset), reg)]
	      | _ => error "[HppaInstr.expand load]"
	    end

	  | expand info (loadf(lab,freg,tmpR),size,_) = let
	       val offset = le_sum(le_label lab,le_const 0)
	    in
		case size
		  of 20 => [i_sdi_ldil(le_hi21 offset,tmpR),
			    i_sdi_ldo(tmpR,le_lo11 offset,tmpR),
			    i_addl(tmpR,base_reg,never,tmpR),
			    
			    i_fldws(tmpR,0,mab_none,freg,FrLeftHalf),
			    i_fldws(tmpR,4,mab_none,freg,FrRightHalf)]
		   | _ => error "loadf"
	    end

	  | expand info (cbranch(dest_lab, skip_lab, cond, r1, r2), size, _) =
	    let val offset = make_jump_offset_expr(dest_lab)
	    in case size of
		4 => [i_sdi_comb(r1, r2, cond, offset, n_no_nullify)]
	      | 8 => [i_sub(r1, r2, invert_cond cond, zero_reg),
		      i_sdi_bl(offset, n_no_nullify, zero_reg)]
	      | 16 => [i_sdi_comb(r1, r2, invert_cond cond,
				  make_jump_offset_expr(skip_lab),
				  n_no_nullify),
		       i_sdi_addil(le_hi21(offset), base_reg),
		       i_sdi_ldo(addr_reg, le_lo11(offset), addr_reg),
		       i_bv(zero_reg, n_no_nullify, addr_reg)]
			   
	      | _ => error "[HppaInstr.expand cbranch]"
	    end

	  | expand info (fbranch(destLab,tmpR),size,loc) = let
	      val offset = make_jump_offset_expr destLab
	    in
		case size 
		  of 4  => [i_sdi_bl(offset,n_no_nullify,zero_reg)]
		   | 12 => [i_sdi_addil(le_hi21 offset, base_reg),
			    i_sdi_ldo(addr_reg,le_lo11 offset,addr_reg),
			    i_bv(zero_reg,n_no_nullify,addr_reg)]
		   | _  => error "[HppaInstr.expand fbranch]" 
	    end

	  | expand info (bbranch(dest_lab, skip_lab, bit, r), size, _) =
	    let val offset = make_jump_offset_expr(dest_lab)
	    in case size of
		4 => [i_sdi_bb(r, bit, sc_leftmost_one, offset, n_no_nullify)]
	      | 8 => [i_sdi_bb(r, bit, sc_leftmost_zero,
			       make_jump_offset_expr(skip_lab), n_no_nullify),
		      i_sdi_bl(offset, n_no_nullify, zero_reg)]
	      | _ => error "[HppaInstr.expand bbranch]"
	    end

	  | expand info (jump(dest_lab, target), size, _) =
	    (case size of
		 4 => [i_sdi_bl(make_jump_offset_expr(dest_lab),
				n_no_nullify, target)]
	       | 12 => let val offset = le_label(dest_lab)
		       in [i_sdi_addil(le_hi21(offset), base_reg),
			   i_sdi_ldo(addr_reg, le_lo11(offset), addr_reg),
			   i_bv(zero_reg, n_no_nullify, addr_reg)]
		       end
	       | _ => error "[HppaInstr.expand jump]")

	 (* if can guarantee that labels in jump table are 0 mod 8
	    then could use i_blr *)

	  | expand info (jumpr(dest_lab, here_lab, offset, treg), size, _) =
	    error "[HppaInstr.expand jumpr]"
	
    end

