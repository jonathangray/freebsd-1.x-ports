(* Copyright 1992 Scott Draves *)

functor HppaCM
    (structure C : CODER
     sharing type C.instruction = HppaInstr.instruction
	 and type C.sdi = HppaInstr.sdi
	     ) : CMACHINE =
struct

    open HppaInstr
    type EA = C.label EA

    fun ea2string (Direct (REG r)) = ("Direct REG " ^ makestring(r))
      | ea2string (FDirect (FREG r)) = ("FDirect FREG " ^ makestring(r))
      | ea2string (Immed r) = ("Immed " ^ makestring(r))
      | ea2string (ImmedLab label) = ("ImmedLab")
	

    datatype condition = EQL | NEQ | GTR | GEQ | LSS | LEQ


    (*
     Reg        ML usage                     C usage
     ----------------------------------------------------------
     0	   	zero (zero-reg)              zero
     1	        reserved (addr_reg)          scratch caller-saves
     2	   	standard argument            rp, return pointer and scratch
     3	   	standard continuation        callee-saves
     4	   	standard closure             callee-saves
     5          standard link                callee-saves
     6-17  	misc regs                    callee-saves
     18	   	temp[1] and heap exhausted   callee-saves
     19	   	limit pointer	             caller-saves
     20	   	varptr                       caller-saves
     21	   	temp[2]                      caller-saves
     22	     	store pointer                caller-saves
     23    	data/alloc pointer           arg3
     24	   	base reg                     arg2
     25	   	temp[3] and mask reg         arg1
     26    	temp[4]                      arg0
     27    	global data pointer          dp, global data pointer
     28	   	???                          result0
     29	   	exnptr	                     static link, milli result, result1
     30	   	C stack pointer              sp, stack pointer
     31	   	gclink register              milli return pointer, or caller-saves
  *)


    local
      open Bits
      val last = ref 0
      and queue = ref (lshift(1,18) +
		       lshift(1,21) +
		       lshift(1,25) +
		       lshift(1,26))
      fun ins i = queue := orb(lshift(1, i), !queue)
      fun remove () =
	  let val l = !last
	      and q = !queue
	      val n = if l = 29 then 0 else l+1
	      val n2 = lshift(1,n)
	  in
	      last := n;
	      if 0 = andb(q, n2) then
		  remove()
	      else
		  (queue := andb(q, notb(n2));
		   n)
	  end
    in
	fun get_tmp_reg () = REG(remove())
	fun free_tmp_reg (REG r) = ins r
    end

    val data_reg      = REG 23  (* the point in the heap where we allocate new data *)
    val limit_reg     = REG 19  (* the end of the heap *)
    val store_reg     = REG 22  (* a list of places we have mutated *)
    val exception_reg = REG 29  (* the current exeption handler *)
    val stack_reg     = REG 30  (* the C stack *)
    val gclink_reg    = REG 31
    (* gclink_reg: when calling GC, we leave our return address here. 
     also used as ML entry point when called from C *)
    val mask_reg      = REG 25  (* when calling GC, put live register mask here *)
    val heap_exhausted_reg = REG 18
    (* valid across fn calls, if this is zero, then do a GC *)

    (* the base_reg is defined in hppadepend.sml.
       it is a pointer to the beginning of the function currently running, plus
       some bias (it points to the register mask: basereg-bias+8 is executable).
       it is used to do PC-relative addressing. it is computed by adding a
       constant to stdlink on function entry.  currently bias=0 *)

    val exnptr = Direct exception_reg
    val varptr = Direct(REG 20)
    val varptr_indexable = true
    val storeptr = Direct store_reg
    val dataptr  = Direct data_reg

    val standardlink    = Direct(REG 5)
    (* stdlink points to the first executable instruction of the code block.
       used to set base reg *)
    val standardarg     = Direct(REG 2)
    val standardcont    = Direct(REG 3)
    val standardclosure = Direct(REG 4)
    val miscregs = map (Direct o REG) 
	[6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]
    val floatregs = map (FDirect o FREG)
	[ 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,
	 18,19,20,21,22,23,24,25,26,27,28,29,30,31]
    val savedfpregs = []
    val arithtemps = []
    val temp1_reg = REG 18
    val temp2_reg = REG 21
    val temp3_reg = REG 25
    val temp4_reg = REG 26

    (* offsets into our C stack frame.  see prim.s for more *)
    val startgc_offset  = ~40
    val mul_addr_offset = ~44
    val div_addr_offset = ~48
    val cvti2d_offset   = ~52
    val arg0_save_offset= ~56
    val arg1_save_offset= ~60


    fun align ()   = ()
    val emit       = C.emit
    val emitSDI    = C.emitSDI
    val emitlong   = C.emitLong
    val realconst  = C.emitReal
    val emitstring = C.emitString
    val immed      = Immed
    val mark       = C.mark
    val comment    = C.comment

    exception BadReal = C.BadReal

    fun emitlab (n, ImmedLab lab) = C.emitLabel(lab, n)
      | emitlab _ = error "[HppaCM.emitlab]"

    val newlabel = ImmedLab o C.newLabel

    fun define (ImmedLab lab) = C.define lab
      | define _ = error "[HppaCM.define]"

    val error = ErrorMsg.impossible

    fun emitLoad32 (n, r) =
	(emit (i_ldil(hi21(n), r));
	 emit (i_ldo(r, lo11(n), r)))

  (** CMachine instructions **)

  (* move (src, dst) *)
    fun move (Immed n, Direct r) =
	if im14(n) then
	    emit (i_ldo(REG 0, n, r))
	else
	    emitLoad32(n, r)

      | move (ImmedLab lab, Direct r) =
	emitSDI(load_addr(lab, 0, r))

      | move (FDirect (fs as FREG _), FDirect (fd as FREG _)) =
	emit (i_fcpy(fs, fpf_double, fd))

      | move (Direct r1, Direct r2) =
	emit (i_or(r1, zero_reg, lc_never, r2))

      | move _ = error "[HppaCM.move]"

  (* ashl (n, x, y) shift left: y <- (x << n), with  n >= 0 *)
    fun ashl (Direct cntR, Direct src, Direct dst) =
	let val t = get_tmp_reg()
	in emit (i_subi(31, cntR, never, t));
	   emit (i_mtctl(t, shift_reg));
	   emit (i_zvdep(src, sc_never, 32, dst));
	   free_tmp_reg(t)
	end

      | ashl (Immed cnt, Direct src, Direct dst) =
	emit (i_zdep(src, sc_never, 31-cnt, 32-cnt, dst))

      | ashl (Direct cntR, Immed src, d as Direct dst) =
	if im5(src) then
	    (emit (i_subi(31, cntR, never, dst));
	     emit (i_mtctl(dst, shift_reg));
	     emit (i_zvdepi(src, sc_never, 32, dst)))
	else
	    let val t = get_tmp_reg()
	    in move(Immed src, Direct t);
		ashl(Direct cntR, Direct t, d);
		free_tmp_reg(t)
	    end

      | ashl (Immed cnt, Immed src, dst) =
	move(Immed(Bits.lshift(src, cnt)), dst)

      | ashl _ = error "[HppaCM.ashl]"

    (* ashr (n, x, y) shift right: y <- (x >> n), with  n >= 0 *)
    fun ashr (Direct cntR, Direct src, Direct dst) =
	let val t = get_tmp_reg()
	in emit (i_subi(31, cntR, never, t));
	    emit (i_mtctl(t, shift_reg));
	    emit (i_vextrs(src, sc_never, 32, dst));
	    free_tmp_reg(t)
	end

      | ashr (Immed cnt, Direct src, Direct dst) =
	emit (i_extrs(src, sc_never, 31-cnt, 32-cnt, dst))

      | ashr (Direct cntR, Immed src, dst) =
	let val t = get_tmp_reg()
	in move (Immed src, Direct t);
	   ashr(Direct cntR, Direct t, dst);
	   free_tmp_reg(t)
	end

      | ashr (Immed cnt, Immed src, dst) =
	move(Immed(Bits.rshift(src, cnt)), dst)

      | ashr _ = error "[HppaCM.ashr]"


    (* jmp (dst) * Unconditional jump to destination. *)
    fun jmp (ImmedLab to_lab) =
	let val from_lab = C.newLabel()
	in C.define from_lab;
	   emitSDI(jump(to_lab, zero_reg))
	end

      | jmp (Direct r) =
	emit (i_bv(zero_reg, n_no_nullify, r))
	
      | jmp _ = error "[HppaCM.jmp]"



    (* should be rewritten to use all the temp registers *)
    fun record(vl, Direct z) =
	let
	    open CPS
	    val len = List.length vl

	    fun f(_,i,nil) = ()

	      | f((t1, t2), i, (Direct r, SELp(j, p)) :: rest) = 
		(* follow ptrs to get the item  *)
		(emit(i_ldw(r, j*4, t1));
		 f((t2, t1), i, (Direct t1, p) :: rest))

	      | f(t, i, (Direct r, OFFp 0) :: rest) = 
		(*  simple store, last first  *) 
		(emit(i_stw(data_reg, i*4, r));
		 f(t, i-1, rest))

	      | f((t1, t2), i, (Direct r, OFFp j) :: rest) = 
		(emit(i_ldo(r, 4*j, t1));
		 f((t2, t1), i, (Direct t1, OFFp 0) :: rest))

	      | f((t1, t2), i, (ea, p) :: rest) =
		(* convert to register-based  *)
		(move(ea, Direct t1);
		 f((t2, t1), i, (Direct t1, p) :: rest))

	    val t1 = get_tmp_reg()
	    and t2 = get_tmp_reg()
	in
	    f((t1,t2),len-1,rev vl);
	    free_tmp_reg t1;
	    free_tmp_reg t2;
	    emit (i_ldo(data_reg, 4, z));
	    emit (i_ldo(data_reg, 4*len, data_reg))
	end
      | record _ = error "HppaCM.record: result not a register"



    fun recordStore (x, y, _) =
	record ([(Immed(System.Tags.make_desc(3, System.Tags.tag_record)), CPS.OFFp 0),
		 (x, CPS.OFFp 0),
		 (y, CPS.OFFp 0),
		 (Direct store_reg, CPS.OFFp 0)],
		Direct store_reg)


    (* select (i, x, y):  y <- mem[x + 4*i] *)
    fun select (i, Direct r, Direct dst) =
	if im14(4*i) then
	    emit (i_ldw(r, 4*i, dst))
	else
	    (* if could guarantee R1 free, would use i_addil here XXX *)
	    let val t = get_tmp_reg()
	    in emitLoad32(i, t);
	       emit (i_ldwx(r, t, ms_shift, dst));
	       free_tmp_reg(t)
	    end

      | select (i, ImmedLab lab, Direct dst) =
	emitSDI(load(lab, i*4, dst))

      | select _ = error "[HppaCM.select]"



    (* offset (i, x, y):  y <- (x + 4*i) *)
    fun offset (i, Direct r, Direct dst) =
	if im14(4*i) then
	    emit (i_addi(4*i, r, never, dst))
	else
	    let val t = get_tmp_reg()
	    in emitLoad32(4*i, t);
	       emit (i_addl(t, r, never, dst));
	       free_tmp_reg(t)
	    end

      | offset (i, ImmedLab lab, Direct dst) =
	emitSDI(load_addr(lab, 4*i, dst))

      | offset _ = error "[HppaCM.offset]"

    (* fetchindexb (x, y, z) fetches an unsigned byte:  y <- mem[x+z] *)
    fun fetchindexb (Direct base, Direct dst, Immed indx) =
	if im14(indx) then
	    emit (i_ldb(base, indx, dst))
	else
	    let val t = get_tmp_reg()
	    in emitLoad32(indx, t);
	       emit (i_ldbx(base, t, ms_shift, dst));
	       free_tmp_reg(t)
	    end

      | fetchindexb (Direct base, Direct dst, Direct indx) =
	emit (i_ldbx(base, indx, ms_shift, dst))

      | fetchindexb _ = error "[HppaCM.fetchindexb]"

    (* storeindexb (x, y, z) stores a byte:  mem[y+z] <- x *)
    fun storeindexb (Direct byte, Direct dst, Immed indx) =
	if im14(indx) then
	    emit (i_stb(dst, indx, byte))
	else
	    let val t1 = get_tmp_reg()
		and t2 = get_tmp_reg()
	    in emitLoad32(indx, t1);
	       emit (i_addl(t1, dst, never, t2));
	       emit (i_stb(t2, 0, byte));
	       free_tmp_reg(t1);
	       free_tmp_reg(t2)
	    end

      | storeindexb (Direct byte, Direct dst, Direct indx) =
	let val t = get_tmp_reg()
	in emit (i_addl(dst, indx, never, t));
	   emit (i_stb(t, 0, byte));
	   free_tmp_reg(t)
	end
    
      | storeindexb (byte as Immed _, dst, indx) =
	let val t = get_tmp_reg()
	in move(byte, Direct t);
	   storeindexb(Direct t, dst, indx);
	   free_tmp_reg(t)
	end

      | storeindexb _ = error "[HppaCM.storeindexb]"

    (* fetchindexl (x, y, z) fetches a word:  y <- mem[x+2*(z-1)] *)
    fun fetchindexl (Direct r1, Direct dst, Direct r2) =
	let val t = get_tmp_reg()
	in ashr(Immed 1, Direct r2, Direct t);
	   emit (i_ldwx(r1, t, ms_shift, dst));
	   free_tmp_reg t
	end

      | fetchindexl (r1, dst, Immed i) =
	select(i div 2, r1, dst)

      | fetchindexl (ImmedLab lab, Direct dst, Direct r) =
	let val t1 = get_tmp_reg()
	    and t2 = get_tmp_reg()
	in emitSDI(load_addr(lab, 0, t1));
	   ashr(Immed 1, Direct r, Direct t2);
	   emit (i_ldwx(t1, t2, ms_shift, dst));
	   free_tmp_reg t1;
	   free_tmp_reg t2
	  end

      | fetchindexl _ = error "[HppaCM.fetchindexl]"

    (* storeindexl (x, y, z) stores a word:  mem[y+2*(z-1)] <- x *)
    (* mem[y+2*(z-1)] = mem[y + 2*z - 2] *)
    fun storeindexl (Direct word, Direct r1, r2 as Direct _) =
	let val t = get_tmp_reg()
	in ashl(Immed 1, r2, Direct t);
	    emit (i_addl(t, r1, never, t));
	    emit (i_stw(t, ~2, word));
	    free_tmp_reg t
	end

      | storeindexl (Direct word, Direct r, Immed i) =
	emit (i_stw(r, 2*(i-1), word))

      | storeindexl (Immed n, x, y) =
	let val t = get_tmp_reg()
	in move(Immed n, Direct t);
	   storeindexl(Direct t, x, y);
	   free_tmp_reg(t)
	end

      (* there should be a faster way XXX *)
      | storeindexl (ImmedLab lab, x, y) =
	let val t = get_tmp_reg()
	in move(ImmedLab lab, Direct t);
	   storeindexl(Direct t, x, y);
	   free_tmp_reg(t)
	end

      | storeindexl (Direct word, ImmedLab lab, Immed i) =
	let val t = get_tmp_reg()
	in emitSDI(load_addr(lab, 2*(i-1), t));
	   emit (i_stw(t, 0, word));
	   free_tmp_reg t
	end

      | storeindexl _ = error "[HppaCM.storeindexl]"


    local

	fun simple_logic oper (Direct r1, Direct r2, Direct dst) =
	    emit (oper(r1, r2, lc_never, dst))

	  | simple_logic oper (Direct r1, n, Direct dst) =
	    let val t = get_tmp_reg()
	    in move(n, Direct t);
	       emit (oper(r1, t, lc_never, dst));
	       free_tmp_reg(t)
	    end

	  | simple_logic oper (n, m as Direct _ , dst) =
	    simple_logic oper (m, n, dst)

	  | simple_logic _ _ = error "[HppaCM.simple_logic]"

    in
	val orb  = simple_logic i_or
	val andb = simple_logic i_and
	val xorb = simple_logic i_xor
    end

    local

	fun simple_arith (op1, op2) (Direct r1, Direct r2, Direct dst) =
	     emit (op1(r1, r2, never, dst))

	  | simple_arith (op1, op2) (Immed n, Direct r, Direct dst) =
	    if im11(n) then
		emit (op2(n, r, never, dst))
	    else
		let val t = get_tmp_reg()
		in move(Immed n, Direct t);
		    emit (op1(t, r, never, dst));
		    free_tmp_reg(t)
		end

	  | simple_arith x (r1 as Direct _, n as Immed _, dst) =
	    simple_arith x (n, r1, dst)

	  (* this case exists for creating constants > 2^29 *)
	  | simple_arith x (n1 as Immed _, n2 as Immed _, dst) =
	    let val t = get_tmp_reg()
	    in move(n1, Direct t);
	       simple_arith x (Direct t, n2, dst);
	       free_tmp_reg(t)
	    end

	  | simple_arith _ _ = error "[HppaCM.simple_arith]"


	(* sub(a,b,c) means c <- b-a.  of course. *)
	fun simple_sub (Immed n, r as Direct _, dst as Direct _) =
	    simple_arith(i_addl, i_addi)(r, Immed(~n), dst)
	  | simple_sub (r1, r2, dst) =
	    simple_arith(i_sub, i_subi)(r2, r1, dst)

	fun simple_subt (Immed n, r as Direct _, dst as Direct _) =
	    simple_arith(i_addo, i_addio)(r, Immed(~n), dst)
	  | simple_subt (r1, r2, dst) =
	    simple_arith(i_subo, i_subio)(r2, r1, dst)
    in
	val add  = simple_arith(i_addl, i_addi)
	val addt = simple_arith(i_addo, i_addio)
	val sub  = simple_sub
	val subt = simple_subt
    end

    fun notb (Direct src, Direct dst) =
	emit (i_uaddcm(zero_reg, src, uc uc_never, dst))

      | notb _ = error "[HppaCM.notb]"


  (* mult/divt:
   * mult (a, b):  b <- (a * b) (with overflow checking done by ml_mul)
   * divt (a, b):  b <- (b div a)
   *
   *)
    local
	
	fun c_op op_addr_offset (a, b) =
	    let val from = C.newLabel()
		and to   = C.newLabel()
		and from2 = C.newLabel()
		and to2   = C.newLabel()
	    in emit(i_ldw(stack_reg, op_addr_offset, temp3_reg));
		move(b, Direct temp1_reg);
		move(a, Direct temp2_reg);

		(* this jump snags the pc *)
		C.define(from);
		emitSDI(jump(to, temp4_reg));

		(* the function returns here, continue on *)
		C.define(from2);
		emitSDI(jump(to2, zero_reg));
		
		(* using return address generated for us, call the function *)
		(* jumping to text seg, set space id XXX *)
		C.define(to);
		emit(i_bv(zero_reg, n_no_nullify, temp3_reg));
		C.define(to2);

		move(Direct temp3_reg, b)
	    end
    in
	val mult = c_op mul_addr_offset
	val divt = c_op div_addr_offset
    end

    (* bbs (i, dst, lab): test the i'th (counting from the lsb) bit
     * of dst and jump to lab if it is zero.  remember HP counts
     * bits from the left 
     *)
    fun bbs (Immed i, Direct r, ImmedLab lab) =
	let val skip_lab = C.newLabel();
	in emitSDI(bbranch(lab, skip_lab, 31-i, r));
	   C.define skip_lab
	end

      | bbs _ = error "[HppaCM.bbs]"

    local
	fun cvt EQL = cc     cc_equal
	  | cvt NEQ = cc_not cc_equal
	  | cvt GTR = cc_not cc_less_or_equal
	  | cvt GEQ = cc_not cc_less
	  | cvt LSS = cc     cc_less
	  | cvt LEQ = cc     cc_less_or_equal
    in
	(* should generate cbranch immediate XXX *)
	fun ibranch (cond, Direct a, Direct b, ImmedLab lab) =
	    let val skip_lab = C.newLabel()
	    in emitSDI (cbranch(lab, skip_lab, cvt cond, a, b));
	       C.define skip_lab
	    end

	  | ibranch (cond, Immed a, Immed b, lab) =
	    if  (case cond of
		     EQL => a=b  | NEQ => a<>b | LSS => a<b |
		     LEQ => a<=b | GTR => a>b  | GEQ => a>=b)
		then jmp(lab)
	    else  ()

	  | ibranch (cond, a as Immed _, b, lab) =
	    let val t = get_tmp_reg();
	    in  move(a, Direct t);
		ibranch(cond, Direct t, b, lab);
		free_tmp_reg t
	    end

	  | ibranch (cond, a, b as Immed _, lab) =
	    let val t = get_tmp_reg();
	    in  move(b, Direct t);
		ibranch(cond, a, Direct t, lab);
		free_tmp_reg t
	    end

	  | ibranch (_,a,b,_) = error "[HppaCM.ibranch]" 
    end

    (* 
     * rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) 
     *)
    fun rangeChk(Immed a, Immed b, lab) =
	if (a < 0) orelse (a >= b) then jmp(lab) else ()

      | rangeChk(Direct a, Direct b, ImmedLab lab) =
	let val skip_lab = C.newLabel()
	in emitSDI (cbranch(lab, skip_lab, cc_not cc_less_unsigned, a, b));
	   C.define skip_lab
	end

      (* should generate cbranch immediate XXX *)
      | rangeChk(a, b as Immed _, lab) =
	  let val t = get_tmp_reg();
	  in  move(b, Direct t);
	      rangeChk(a, Direct t, lab);
	      free_tmp_reg t
	  end

      (* should generate cbranch immediate XXX *)
      | rangeChk(a as Immed _, b, lab) =
	  let val t = get_tmp_reg();
	  in  move(a, Direct t);
	      rangeChk(Direct t, b, lab);
	      free_tmp_reg t
	  end

      | rangeChk _ = error "[HppaCM.rangeChk]"

    local
      val real_tag = System.Tags.desc_reald

      fun store_float(FDirect fp,Direct dst,offset) = 
	     (emit(i_fstws(dst,offset,mab_none,fp,FrLeftHalf));
	      emit(i_fstws(dst,offset+4,mab_none,fp,FrRightHalf)))
	| store_float _ = error "[HppaCM.store_float]"

      fun load_float(fp,Direct src) = 
	     (emit(i_fldws(src,0,mab_none,fp,FrLeftHalf));
	      emit(i_fldws(src,4,mab_none,fp,FrRightHalf)))
	| load_float(fp,ImmedLab lab) = let
	     val tmpR = get_tmp_reg()
          in
	      emitSDI(loadf(lab,fp,tmpR));
	      free_tmp_reg tmpR
	  end
	| load_float _ = error "HppaCM.load_float"
    in
	fun storefloat (fpr,gpr as Direct gp) = let
              val tmpR = get_tmp_reg()
            in
		store_float(fpr,Direct data_reg,4);
		move(Immed real_tag, Direct tmpR);
		emit(i_stw(data_reg, 0, tmpR));
		emit(i_addi(4, data_reg, never, gp));
		emit(i_addi(12, data_reg, never, data_reg));
		free_tmp_reg tmpR
            end
	  | storefloat _ = error "[HppaCM.storefloat]"

	fun loadfloat(src,FDirect dst) = load_float(dst,src)
	  | loadfloat _ = error "[HppaCM.loadfloat]"

        (* fetchindexd(x,y,z): y <- mem[x+4*(z-1)] *)
	fun fetchindexd(Direct x, FDirect fp, Direct z) =
	    let val t = get_tmp_reg()
	    in ashr(Immed 1, Direct z, Direct t);
	       emit (i_flddx(x, t, ms_shift, fp));
	       free_tmp_reg(t)
	    end

	  | fetchindexd(Direct x, FDirect fp, Immed i) =
	    let val offset = 4*(i-1)
	    in if im5(offset) then
		emit (i_fldds(x, offset, mab_none, fp))
	       else
		   let val t = get_tmp_reg()
		       in move(Immed offset, Direct t);
			  emit (i_addl(t, x, never, t));
			  emit (i_flddx(t, zero_reg, ms_none, fp));
			  free_tmp_reg(t)
		   end
	    end

	  | fetchindexd _ = error "[HppaCM.fetchindexd]"

      (* storeindexd: mem[y+4*(z-1)] <- x *)
	fun storeindexd (FDirect fp, Direct y, Direct z) =
	    let val t = get_tmp_reg()
	    in ashr(Immed 1, Direct z, Direct t);
	       emit (i_fstdx(y, t, ms_shift, fp));
	       free_tmp_reg(t)
	    end

	  | storeindexd (FDirect fp, Direct y, Immed i) =
	    let val offset = 4*(i-1)
	    in if im5(offset) then
		emit (i_fstds(y, offset, mab_none, fp))
	       else
		   let val t = get_tmp_reg()
		       in move(Immed offset, Direct t);
			  emit (i_addl(t, y, never, t));
			  emit (i_fstdx(t, REG 0, ms_none, fp));
			  free_tmp_reg(t)
		   end
	    end

	  | storeindexd _ = error "[HppaCM.storeindexd]"


    end

    local

	fun floatOp oper (FDirect fpr1, FDirect fpr2, FDirect fpr3) =
	    emit (oper(fpr1, fpr2, fpf_double, fpr3))
	  | floatOp _ _ = error "[HppaCM.floatOp]"
    in

	val faddd = floatOp i_fadd
	val fsubd = floatOp i_fsub
	val fmuld = floatOp i_fmpy
	val fdivd = floatOp i_fdiv
    end

    fun fnegd (FDirect r, FDirect d) = emit (i_fsub(FREG 0, r, fpf_double, d))
      | fnegd _                      = error "[HppaCM.fnegd]"

    fun fabsd (FDirect r, FDirect d) = emit (i_fabs(r, fpf_double, d))
      | fabsd _                      = error "[HppaCM.fabsd]"

    fun cvti2d (Direct r, FDirect f) = let
	  val tmpR = get_tmp_reg()
        in
	    emit(i_ldo(zero_reg,cvti2d_offset,tmpR));
	    emit(i_stw(stack_reg, cvti2d_offset, r));
	    emit(i_fldwx(stack_reg,tmpR,ms_none,f,FrLeftHalf));
	    emit(i_fcnvxf(f,fpf_single,f,fpf_double));
	    free_tmp_reg tmpR
        end
      | cvti2d (i as Immed _, f as FDirect _) = let
	   val t = get_tmp_reg()
	in 
	    move(i, Direct t);
	    cvti2d(Direct t, f);
	    free_tmp_reg(t)
	end
      | cvti2d _ = error "[HppaCM.cvti2d]"

    local
	fun mkFpc (eq,gt,lt) = 
	    fpc{equal=eq,greater=gt,less=lt,trap=false,unordered=false}

	fun cvt EQL = mkFpc (false,true, true)
	  | cvt NEQ = mkFpc (true, false,false)
	  | cvt GTR = mkFpc (true, false,true)
	  | cvt GEQ = mkFpc (false,false,true)
	  | cvt LSS = mkFpc (true, true, false)
	  | cvt LEQ = mkFpc (false,true, false)
    in
       fun fbranchd(cond,FDirect a,FDirect b,ImmedLab lab) = let
	       val tmpR = get_tmp_reg()
	   in
	       emit(i_fcmp(a,b,fpf_double,cvt cond));
	       emit(i_ftest);
	       emitSDI(fbranch(lab,tmpR));
	       free_tmp_reg tmpR
	   end
	 | fbranchd _ = error "[HppaCM.fbranchd]"
    end

    (* jmpindexb (x):  pc <- (x+y) *)
    (* XXX should use i_blr.  see jumpr sdi. *at least* it should
       use the index feature of i_bv *)
    fun jmpindexb(ImmedLab to_lab, y) =
	let val t = get_tmp_reg()
	    val dt = Direct t
	in emitSDI(load_addr(to_lab, 0, t));
	   add(dt, y, dt);
	   jmp(dt);
	   free_tmp_reg t
	end

      | jmpindexb _ = error "[HppaCM.jmpindexb]"


    fun testLimit() =
	(emit (i_comclr(data_reg, limit_reg, cc_not cc_less,
			heap_exhausted_reg));
	 emit (i_ldo(zero_reg, 1, heap_exhausted_reg)))

    (* this assumes the temp registers are free to use *)
    fun checkLimit (max_alloc, restart, mask) =
	let val skip_lab = C.newLabel()
	    and from_lab = C.newLabel()
	    and skip_lab' = C.newLabel()
	    and n = max_alloc - 4096
	in
	    if (n > 0) then
		(add(Direct data_reg, Immed n, Direct heap_exhausted_reg);
		 C.define from_lab;
		 emitSDI(cbranch(skip_lab, skip_lab', cc cc_less,
				 heap_exhausted_reg, limit_reg));
		 C.define skip_lab')
	    else (C.define from_lab;
		  emitSDI(cbranch(skip_lab, skip_lab', cc_not cc_equal,
				  heap_exhausted_reg, zero_reg));
		  C.define skip_lab');

	    emit(i_ldw(stack_reg, startgc_offset, temp2_reg));
	    move(mask, Direct mask_reg);
	    move(restart, Direct gclink_reg);
	    emit(i_bv(zero_reg, n_no_nullify, temp2_reg));
	    C.define(skip_lab)
	end

    fun beginStdFn (ImmedLab lab, Direct reg) =
	emitSDI(set_base_reg(lab, reg))

      | beginStdFn _ = error "[HppaCM.beginStdFn]"

end (* functor HppaCM *)

