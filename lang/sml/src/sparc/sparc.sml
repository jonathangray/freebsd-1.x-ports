(* sparc.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *)

functor SparcCM (
    structure C : CODER
    sharing type C.instruction = SparcInstr.instruction
        and type C.sdi = SparcInstr.sdi) : CMACHINE =
  struct

    structure C' : sig
	eqtype label
	val mark : unit -> unit
	val comment : string -> unit
	exception BadReal of string
    end = C
    open C'

    structure S' : sig
	datatype register = REG of int
	datatype fregister = FREG of int
	datatype 'label labelexp
	  = LABELexp of {       (* An offset relative to a label.  The value of a *)
	      base : 'label,    (* label expression is ((dst - base) + offset). *)
	      dst : 'label,
	      offset : int
	    }
	datatype 'label operand
	  = REGrand of register     (* A register value *)
	  | IMrand of int           (* A small integer constant (13 bits) *)
	  | LABrand of 'label labelexp     (* A small valued label expression (13 bits) *)
	  | HIrand of 'label labelexp      (* The high 22 bits of a label expression *)
	  | LOrand of 'label labelexp      (* The low 10 bits of a label expression *)
	datatype cond_code
	  = CC_A | CC_E | CC_NE | CC_G | CC_GE | CC_L | CC_LE | CC_GEU | CC_LEU
      end = SparcInstr
    open S'

    val zeroR = REG 0                       (* %g0 *)
    val zeroRand = REGrand zeroR

    local

      fun emit_ld args = C.emit (SparcInstr.I_ld args)
      fun emit_ldb args = C.emit (SparcInstr.I_ldb args)
      fun emit_ldf args = C.emit (SparcInstr.I_ldf args)
      fun emit_st args = C.emit (SparcInstr.I_st args)
      fun emit_stb args = C.emit (SparcInstr.I_stb args)
      fun emit_stf args = C.emit (SparcInstr.I_stf args)
      fun emit_sethi args = C.emit (SparcInstr.I_sethi args)
      fun emit_bcc args = C.emit (SparcInstr.I_bcc args)
      fun emit_fbcc args = C.emit (SparcInstr.I_fbcc args)
      fun emit_jmpl args = C.emit (SparcInstr.I_jmpl args)
      fun emit_jmp (r, offset) = C.emit (SparcInstr.I_jmpl(r, offset, zeroR))
      fun emit_add args = C.emit (SparcInstr.I_add args)
      fun emit_addcc args = C.emit (SparcInstr.I_addcc args)
      fun emit_taddcctv args = C.emit (SparcInstr.I_taddcctv args)
      fun emit_sub args = C.emit (SparcInstr.I_sub args)
      fun emit_subcc args = C.emit (SparcInstr.I_subcc args)
      fun emit_sra args = C.emit (SparcInstr.I_sra args)
      fun emit_sll args = C.emit (SparcInstr.I_sll args)
      fun emit_and args = C.emit (SparcInstr.I_and args)
      fun emit_andcc args = C.emit (SparcInstr.I_andcc args)
      fun emit_or args = C.emit (SparcInstr.I_or args)
      fun emit_xor args = C.emit (SparcInstr.I_xor args)
      fun emit_not args = C.emit (SparcInstr.I_not args)
      fun emit_tvs () = C.emit SparcInstr.I_tvs
      fun emit_fadd args = C.emit (SparcInstr.I_fadd args)
      fun emit_fsub args = C.emit (SparcInstr.I_fsub args)
      fun emit_fmul args = C.emit (SparcInstr.I_fmul args)
      fun emit_fdiv args = C.emit (SparcInstr.I_fdiv args)
      fun emit_fneg args = C.emit (SparcInstr.I_fneg args)
      fun emit_fabs args = C.emit (SparcInstr.I_fabs args)
      fun emit_fcmp args = C.emit (SparcInstr.I_fcmp args)
      fun emit_fmov args = C.emit (SparcInstr.I_fmov args)
      fun emit_fitod args = C.emit (SparcInstr.I_fitod args)

      local
	fun mkLabExp (lab, n) = LABELexp{base= C.baseLab, dst= lab, offset= (n-4096)}
      in

      fun setBaseAddr (lab, reg) = 
	       C.emitSDI (
	        SparcInstr.SetBaseAddr(mkLabExp(lab, 0), reg))

      fun loadAddr (lab, n, dst) = (
	    C.emitSDI (SparcInstr.LoadAddr(mkLabExp(lab, n), dst)))

      fun load (lab, n, dst, tmpR) = (
	    C.emitSDI (SparcInstr.Load(mkLabExp(lab, n), dst, tmpR)))

      fun loadF (lab, n, dst, tmpR) = (
	    C.emitSDI (SparcInstr.LoadF(mkLabExp(lab, n), dst, tmpR)))

      end (* local *)

    in

    datatype EA
      = Immed of int
      | ImmedLab of label
      | Direct of register
      | FDirect of fregister

    datatype condition = EQL | NEQ | GTR | GEQ | LSS | LEQ

    val immed = Immed

  (** Dedicated registers **)
    val exnptr = Direct(REG 7)              (* %g7 *)
    val storeptr = Direct(REG 5)            (* %g5 *)
    val arithtemps = []
    val varptr = Direct(REG 29)             (* %i5 *)
    val varptr_indexable = true
    val standardclosure = Direct(REG 26)    (* %i2 *)
    val standardarg = Direct(REG 24)        (* %i0 *)
    val standardcont = Direct(REG 25)       (* %i1 *)
    val standardlink = Direct(REG 1)        (* %g1 *)
    val miscregs = map (Direct o REG) [     (* %g2-%g3, %o0-%o1, %l0-%l7, %i4 *)
	  2, 3, 8, 9, 16, 17, 18, 19, 20, 21, 22, 23, 28
	  ]
	(*
	 * cc treats none of the floating point registers as callee save.
	 *)
    val savedfpregs = [] : EA list
    val floatregs = let fun from(n,m) = if n > m then [] else n :: from(n+2,m)
		    in map (FDirect o FREG) (from(0,31))
		    end
    val dataptrR = REG 6                    (* %g6 *)
    val limitptrR = REG 4                   (* %g4 *)
    val limitptrRand = REGrand limitptrR
  (* the following two registers are used for calling ml_mul & ml_div *)
    val spR = REG 14                        (* %sp (%o6) *)
    val linkR = REG 15                      (* %o7, link register *)
    val maskR = REG 13       (* %o5, also used as temporary *)
    val checkR = REG 12      (* %o4, also used as temporary *)

  (** Temporary registers **
   * We use registers %o2-%o5 as temporaries.  They are used in a round-robin
   * order to facilitate instruction scheduling.
   *)
    local
      val rear = ref 0 and queue = ref 0
      fun ins i = let
	    val r = !rear
	    in
	      queue := Bits.orb(Bits.lshift(i, r), !queue);
	      rear := r + 5
	    end
      fun remove () = let
	    val q = !queue
	    val x = Bits.andb (q, 31)
	    in
	      queue := Bits.rshift (q, 5);
	      rear := !rear - 5;
	      x
	    end
      val _ = app ins [10, 11, 12, 13]      (* %o2-%o5 *)
    in

  (* Registers %o2, %o3 & %o4 are also used to call ml_mul and ml_div. *)
    val arg1EA = Direct(REG 10) and arg2EA = Direct(REG 11)
    val opAddrR = REG 12

  (* Get a temporary register. *)
    fun getTmpReg () = REG(remove())

   (* If r is a temporary register, then free it. *)
    fun freeReg (REG r) = if ((9 < r) andalso (r < 14)) then (ins r) else ()

  (* Free a temporary register. *)
    fun freeTmpReg (REG r) = ins r

    end (* local *)


  (* align is a nop, since strings are automatically padded. *)
    fun align () = ()

    val emitlong = C.emitLong
    val realconst = C.emitReal
    val emitstring = C.emitString

    fun emitlab (n, ImmedLab lab) = C.emitLabel (lab, n)
      | emitlab _ = ErrorMsg.impossible "[SparcCM.emitlab]"

    val newlabel = ImmedLab o C.newLabel
    fun define (ImmedLab lab) = C.define lab
      | define _ = ErrorMsg.impossible "[SparcCM.define]"

    datatype immed_size = Immed13 | Immed32

    fun sizeImmed n = if (~4096 <= n) andalso (n < 4096) then Immed13 else Immed32


  (** Utility operations **)

    fun emitMove (src, dst) = emit_or (zeroR, REGrand src, dst)

    fun loadImmed32 (n, r) = let
	  val lo10 = Bits.andb(n, 1023)
	  in
	    emit_sethi (IMrand(Bits.rshift(n, 10)), r);
	    if (lo10 <> 0) then emit_or(r, IMrand lo10, r) else ()
	  end

    fun loadImmed (n, r) = (
	  case (sizeImmed n)
	   of Immed13 => emit_or(zeroR, IMrand n, r)
	    | Immed32 => loadImmed32 (n, r))

    fun op32 f (r1, n, r2) = let val tmpR = getTmpReg()
	  in
	    loadImmed32 (n, tmpR);
	    f (r1, REGrand tmpR, r2);
	    freeTmpReg tmpR
	  end

    fun loadReg(r, offset, dst) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_ld (r, IMrand offset, dst)
	    | Immed32 => (op32 emit_ld) (r, offset, dst))

    fun store (src, r, offset) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_st (r, IMrand offset, src)
	    | Immed32 => (op32 emit_st) (r, offset, src))

    fun addImmed (r, n, dst) = (
	  case (sizeImmed n)
	   of Immed13 => emit_add (r, IMrand n, dst)
	    | Immed32 => (op32 emit_add) (r, n, dst))

    fun compareImmed (r, n) = (
	  case (sizeImmed n)
	   of Immed13 => emit_subcc (r, IMrand n, zeroR)
	    | Immed32 => (op32 emit_subcc) (r, n, zeroR))

    fun sparcCC EQL = CC_E | sparcCC NEQ = CC_NE
      | sparcCC GTR = CC_G | sparcCC GEQ = CC_GE
      | sparcCC LSS = CC_L | sparcCC LEQ = CC_LE


  (** CMachine instructions **)

  (* move (src, dst) *)
    fun move (Immed n, Direct r) = loadImmed (n, r)
      | move (ImmedLab lab, Direct r) = loadAddr (lab, 0, r)
      | move (FDirect (FREG fs), FDirect (FREG fd)) = let
	  fun even x = (Bits.andb(x, 0x1) = 0)
	  in
	    if (even fs andalso even fd)
	      then (
		emit_fmov(FREG fs, FREG fd);
		emit_fmov(FREG (fs+1), FREG (fd+1)))
	      else ErrorMsg.impossible "[SparcCM.move: bad floating point registers]"
	end
      | move (Direct r1, Direct r2) = emitMove (r1, r2)
      | move _ = ErrorMsg.impossible "[SparcCM.move]"

   fun testLimit () = emit_subcc (dataptrR, limitptrRand, zeroR)

   val startgc_offset = 88

  (* checkLimit (n):
   * Generate code to check the heap limit to see if there is enough free space
   * to allocate n bytes.
   *)
  fun checkLimit(max_allocation, restart, mask) =
      (* NOTE: THIS CODE USES TEMP REGS BY ALIASES.
       Thus it is important that none of the emitted pseudo-instructions
       below uses getTmpReg(), directly or indirectly. *)
    let val lab' = C.newLabel()
	val n = max_allocation - 4096
     in if n > 0
        then if n<2048
	     then (emit_add(dataptrR,IMrand n,checkR);
		   emit_subcc(checkR, limitptrRand, zeroR))
	     else (emit_sethi(IMrand(Bits.rshift(n,10)),checkR);
		   emit_or(checkR, IMrand(Bits.andb(n,1023)), checkR);
		   emit_add(dataptrR, REGrand checkR, checkR);
		   emit_subcc(checkR, limitptrRand, zeroR))
        else ();
	emit_bcc (CC_LE, lab');
	loadReg(spR, startgc_offset, checkR);
        move(mask, Direct maskR);
	move(restart, Direct linkR);
	emit_jmp (checkR, zeroRand);
        C.define lab'
    end

  (* beginStdFn ():
   * Note the beginning of a standard function.  This requires generating 
   * code to load the base code block address into baseCodePtr.
   *)
    fun beginStdFn(ImmedLab lab, Direct reg) = setBaseAddr(lab,reg)

  (* jmp (dst):
   * Unconditional jump to destination.
   *)
    fun jmp (ImmedLab lab) = emit_bcc (CC_A, lab)
      | jmp (Direct r) = emit_jmp (r, zeroRand)
      | jmp _ = ErrorMsg.impossible "[SparcCM.jmp]"

  (* record (vl, dst):
   * makes a new record, puts address of it into the destination specified
   * by the second arg. The contents are numbered from ~1 and up.
   *)
    fun record (vl : (EA * CPS.accesspath) list, Direct dst) = let
	  val len = length vl
	  val minBlockSize = 6
	(* generate code to move one or more adjacent fields from one record into
	 * adjacent fields in the new record.  If the block is big enough, then
	 * use a block copy loop.
	 *)
	  fun blockMove (srcR, startindx, path, offset) = let
	      (* check a CPS path to see how large the block is *)
		fun chkpath (cnt, i,
		    path as (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) =
		      if (r = srcR) andalso (i+offset = j)
			then chkpath (cnt+1, i-1, rest)
			else (cnt, path)
		  | chkpath (cnt, _, rest) = (cnt, rest)
	      (* generate code to move fields individually *)
		fun moveFields (0, _) = ()
		  | moveFields (n, indx) = let val tmpR = getTmpReg()
		      in
			loadReg(srcR, (indx+offset)*4, tmpR);
			store (tmpR, dataptrR, indx*4);
			freeTmpReg tmpR;
			moveFields(n-1, indx-1)
		      end
		val (blksz, rest) = chkpath(1, startindx-1, path)
		in
		  if (blksz < minBlockSize)
		    then moveFields(blksz, startindx)
		    else if (offset = 0)
		      then let
			val lab = C.newLabel()
			val indxR = getTmpReg() and tmpR = getTmpReg()
			in
			  loadImmed (startindx*4, indxR);
			  C.define lab;
			  emit_ld (srcR, REGrand indxR, tmpR);
			  compareImmed (indxR, (startindx-blksz)*4);
			  emit_st (dataptrR, REGrand indxR, tmpR);
			  emit_sub (indxR, IMrand 4, indxR);
			  emit_bcc (CC_G, lab);
			  freeTmpReg indxR; freeTmpReg tmpR
			end
		      else let
			val lab = C.newLabel()
			val indxR1 = getTmpReg() and indxR2 = getTmpReg()
			val tmpR = getTmpReg()
			in
			  loadImmed ((startindx+offset)*4, indxR1);
			  loadImmed (startindx*4, indxR2);
			  C.define lab;
			  emit_ld (srcR, REGrand indxR1, tmpR);
			  emit_sub (indxR1, IMrand 4, indxR1);
			  emit_st (dataptrR, REGrand indxR2, tmpR);
			  emit_sub (indxR2, IMrand 4, indxR2);
			  compareImmed (indxR1, (startindx+offset-blksz)*4);
			  emit_bcc (CC_G, lab);
			  freeTmpReg indxR1; freeTmpReg indxR2; freeTmpReg tmpR
			end;
		  freeReg srcR;
		  (startindx-blksz, rest)
		end (* blockMove *)
      (* For each field in the record generate the necessary moves to initialize
       * it in the new record.
       *)
	  fun fields (_, nil) = ()
	    | fields (i, (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) =
		fields (blockMove (r, i, rest, j-i))
	    | fields (i, (Direct r, CPS.SELp(j, p)) :: rest) = let
		val tmpR = getTmpReg()
		in
		  loadReg(r, j*4, tmpR);
		  freeReg r;
		  fields (i, (Direct tmpR, p) :: rest)
		end
	    | fields (i, (Direct r, CPS.OFFp 0) :: rest) = (
		store (r, dataptrR, i*4);
		freeReg r;
		fields (i-1, rest))
	    | fields (i, (Direct r, CPS.OFFp j) :: rest) = let
		val tmpR = getTmpReg()
		val offset = j*4
		in
		  case sizeImmed offset
		   of Immed13 => emit_add (r, IMrand offset, tmpR)
		    | Immed32 => (
			loadImmed32 (offset, tmpR);
			emit_add (r, REGrand tmpR, tmpR))
		  (* end case *);
		  store (tmpR, dataptrR, i*4);
		  freeTmpReg tmpR; freeReg r;
		  fields (i-1, rest)
		end
	    | fields (i, (x, p) :: rest) =  let
		val tmpR = getTmpReg()
		in
		  move (x, Direct tmpR);
		  fields (i, (Direct tmpR, p) :: rest)
		end
	  in
	    fields (len-2, rev vl);
	    emitMove (dataptrR, dst);
	    addImmed (dataptrR, len*4, dataptrR)
	end
      | record _ = ErrorMsg.impossible "[SparcCM.record]"

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(z-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, _) = record ([
	    (Immed(System.Tags.make_desc(3, System.Tags.tag_record)), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)

  (* select (i, x, y):  y <- mem[x + 4*i] *)
    fun select (i, Direct r, Direct dst) = loadReg(r, i*4, dst)
      | select (i, ImmedLab lab, Direct dst) = let val tmpR = getTmpReg()
	  in
	    load (lab, i*4, dst, tmpR);
	    freeTmpReg tmpR
	  end
      | select _ = ErrorMsg.impossible "[SparcCM.select]"

  (* offset (i, x, y):  y <- (x + 4*i) *)
    fun offset (i, Direct r, Direct dst) = addImmed (r, 4*i, dst)
      | offset (i, ImmedLab lab, Direct dst) = loadAddr (lab, i, dst)
      | offset _ = ErrorMsg.impossible "[SparcCM.offset]"

    local
      fun moveByte movFn = let
	    fun mov (Direct r, Direct base, Direct indx) = movFn(base, REGrand indx, r)
	      | mov (Direct r, Direct base, Immed indx) = (
		  case (sizeImmed indx)
		   of Immed13 => movFn (base, IMrand indx, r)
		    | Immed32 => (op32 movFn) (base, indx, r))
	      | mov _ = ErrorMsg.impossible "[SparcCM.moveByte]"
	    in
	      mov
	    end
      val loadByte = moveByte emit_ldb
      val storeByte = moveByte emit_stb
    in

  (* fetchindexb (x, y, z) fetches an unsigned byte:  y <- mem[x+z] *)
    fun fetchindexb (base, dst, indx) = loadByte(dst, base, indx)

  (* storeindexb (x, y, z) stores a byte:  mem[y+z] <- x *)
    fun storeindexb (Immed i, base, indx) = let
	  val tmpR = getTmpReg()
	  in
	    loadImmed (i, tmpR);
	    storeByte (Direct tmpR, base, indx);
	    freeTmpReg tmpR
	  end
      | storeindexb arg = storeByte arg
    end (* local *)

  (* jmpindexb (x):  pc <- (x+y) *)
    fun jmpindexb(ImmedLab lab, Direct y) = let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, 0, tmpR1);
	    emit_jmp (tmpR1, REGrand y);
	    freeTmpReg tmpR1
	  end
      | jmpindexb _ = ErrorMsg.impossible "[SparcCM.jmpindexb]"

  (* fetchindexl (x, y, z) fetches a word:  y <- mem[x+2*(z-1)] *)
    fun fetchindexl (Direct r1, Direct dst, Direct r2) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sub (r2, IMrand 1, tmpR);
	    emit_add (tmpR, REGrand tmpR, tmpR);
	    emit_ld (r1, REGrand tmpR, dst);
	    freeTmpReg tmpR
	  end
      | fetchindexl (Direct r1, Direct dst, Immed i) = loadReg(r1, 2*(i-1), dst)
      | fetchindexl (ImmedLab lab, Direct dst, Direct r) =  let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, ~2, tmpR1);
	    emit_add (r, REGrand tmpR1, tmpR1);
	    emit_ld (r, REGrand tmpR1, dst);
	    freeTmpReg tmpR1
	  end
      | fetchindexl _ = ErrorMsg.impossible "[SparcCM.fetchindexl]"

  (*storeindexl (x, y, z) stores a word:  mem[y+2*(z-1)] <- x *)
    fun storeindexl (Direct src, Direct r1, Direct r2) = let val tmpR = getTmpReg()
	  in
	    emit_sub (r2, IMrand 1, tmpR);
	    emit_add (tmpR, REGrand tmpR, tmpR);
	    emit_st (r1, REGrand tmpR, src);
	    freeTmpReg tmpR
	  end
      | storeindexl (Direct src, Direct r, Immed i) = store (src, r, 2*(i-1))
      | storeindexl (Immed n, x, y) = let val tmpR = getTmpReg()
	  in
	    loadImmed (n, tmpR);
	    storeindexl (Direct tmpR, x, y);
	    freeTmpReg tmpR
	  end
      | storeindexl (ImmedLab lab, x, y) = let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, 0, tmpR1);
	    storeindexl (Direct tmpR1, x, y);
	    freeTmpReg tmpR1
	  end
(** NOTE: in a sane world the following case would be unecessary, but it
 ** is used in an ugly profiling hack.
 **)
      | storeindexl (Direct src, ImmedLab lab, Immed i) = let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, 2*(i-1), tmpR1);
	    emit_st (tmpR1, zeroRand, src);
	    freeTmpReg tmpR1
	  end
      | storeindexl _ = ErrorMsg.impossible "[SparcCM.storeindexl]"


   (* fetchindexd(x,y,z): y <- mem[x+4*(z-1)] *)
    fun fetchindexd(Direct x, FDirect(FREG fp), Direct z) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sll (z, IMrand 2, tmpR);
	    emit_add (tmpR, REGrand x, tmpR);
	    emit_ldf (tmpR, IMrand ~4, FREG fp);
	    emit_ldf (tmpR, zeroRand, FREG(fp+1));
	    freeTmpReg tmpR
	  end
      | fetchindexd(Direct x, FDirect(FREG fp), Immed i) = let
	  val offset = 4*(i-1)
	  in
	    case sizeImmed (offset+4) 
	     of Immed13 => (
		  emit_ldf(x, IMrand offset, FREG fp);
		  emit_ldf(x, IMrand(offset+4), FREG(fp+1)))
	      | Immed32 => let val tmpR = getTmpReg()
		  in
		    loadImmed(offset,tmpR);
		    emit_add(x,REGrand tmpR,tmpR);
		    emit_ldf(tmpR,zeroRand,FREG fp);
		    emit_ldf(tmpR,IMrand 4,FREG(fp+1));
		    freeTmpReg tmpR
		  end
          end
      | fetchindexd _ = ErrorMsg.impossible "[SparcCM.fetchindexd]"

  (* storeindexd: mem[y+4*(z-1)] <- x *)
    fun storeindexd (FDirect(FREG fp), Direct y, Direct z) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sll (z, IMrand 2, tmpR);
	    emit_add (tmpR, REGrand y, tmpR);
	    emit_stf (tmpR, IMrand ~4, FREG fp);
	    emit_stf (tmpR ,zeroRand, FREG (fp+1));
	    freeTmpReg tmpR
	  end
      | storeindexd (FDirect(FREG fp), Direct y, Immed i) = let
	  val offset = 4*(i-1)
	  in
	    case (sizeImmed (offset+4))
	     of Immed13 => (
		  emit_stf (y, IMrand offset, FREG fp);
		  emit_stf (y, IMrand (offset+4), FREG(fp+1)))
	      | Immed32 => let
		  val tmpR = getTmpReg()
		  in
		    loadImmed(offset,tmpR);
		    emit_add(y,REGrand tmpR,tmpR);
		    emit_stf(tmpR,zeroRand,FREG fp);
		    emit_stf(tmpR,IMrand 4,FREG(fp+1));
		    freeTmpReg tmpR
		  end
	  end
      | storeindexd _ = ErrorMsg.impossible "[SparcCM.storeindexd]"

  (* ashl (n, x, y) shift left: y <- (x << n), with  n >= 0 *)
    fun ashl (Direct cntR, Direct src, Direct dst) =
	  emit_sll(src, REGrand cntR, dst)
      | ashl (Immed cnt, Direct src, Direct dst) =
	  emit_sll (src, IMrand(Bits.andb(cnt, 31)), dst)
      | ashl (Direct cntR, Immed src, Direct dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    emit_sll (tmpR, REGrand cntR, dst);
	    freeTmpReg tmpR
	  end
      | ashl (Immed cnt, Immed src, Direct dst) = (
	  loadImmed (Bits.lshift(src, cnt), dst))
      | ashl _ = ErrorMsg.impossible "[SparcCM.ashl]"

  (* ashr (n, x, y) shift right: y <- (x >> n), with  n >= 0 *)
    fun ashr (Direct cntR, Direct src, Direct dst) =
	  emit_sra (src, REGrand cntR, dst)
      | ashr (Immed cnt, Direct src, Direct dst) =
	  emit_sra (src, IMrand(Bits.andb(cnt, 31)), dst)
      | ashr (Direct cntR, Immed src, Direct dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    emit_sra (tmpR, REGrand cntR, dst);
	    freeTmpReg tmpR
	  end
      | ashr (Immed cnt, Immed src, Direct dst) = (
	  loadImmed (Bits.rshift(src, cnt), dst))
      | ashr _ = ErrorMsg.impossible "[SparcCM.ashr]"

    local
	fun adjArgs f (a as Immed _, b, c) = f (b, a, c)
	  | adjArgs f args = f args
	fun adjSubArgs f (a, Immed 0, c) = f(Direct(zeroR), a, c)
	  | adjSubArgs f (a, Immed b, c) = let val tmpR = getTmpReg()
	      in
		loadImmed (b, tmpR);
		f (Direct tmpR, a, c);
		freeTmpReg tmpR
	      end
	  | adjSubArgs f (a, b, c) = f (b, a, c)
	fun arithOp f (Direct r1, Direct r2, Direct dst) = f (r1, REGrand r2, dst)
	  | arithOp f (Direct r, Immed n, Direct dst) = (
	      case (sizeImmed n)
	       of Immed13 => f (r, IMrand n, dst)
		| Immed32 => let val tmpR = getTmpReg()
		    in
		      loadImmed32 (n, tmpR);
		      f (r, REGrand tmpR, dst);
		      freeTmpReg tmpR
		    end)
	  | arithOp _ _ = ErrorMsg.impossible "[SparcCM.arithOp]"
	val addt' = adjArgs (arithOp (fn args => (emit_addcc args; emit_tvs())))
    in

    val orb = adjArgs (arithOp emit_or)
    val andb = adjArgs (arithOp emit_and)
    val xorb = adjArgs (arithOp emit_xor)
    fun notb (Direct src, Direct dst) = emit_not (src, dst)
      | notb _ = ErrorMsg.impossible "[SparcCM.notb]"

    val add = adjArgs (arithOp emit_add)
    fun addt (Immed a, b as Immed _, dst) = let val tmpR = getTmpReg ()
	(* This should only occur when we need to build a constant larger than
	 * 2^29.  Note, we assume that "b" is tagged (see "cps/generic.sml").
	 *)
	  in
	    loadImmed (a, tmpR);
	    addt' (Direct tmpR, b, dst);
	    freeTmpReg tmpR
	  end
      | addt args = addt' args

    val op sub = adjSubArgs (arithOp emit_sub)
    val subt = adjSubArgs (arithOp (fn args => (emit_subcc args; emit_tvs())))

    end (* local *)

  (* mult/divt:
   * mult (a, b):  b <- (a * b) (with overflow checking done by ml_mul)
   * divt (a, b):  b <- (b div a)
   *)
    local
      (* call an off-line arithmetic routine. *)
	fun intOp opAddrOffset (a, b as Direct _) = (
	      emit_ld (spR, opAddrOffset, opAddrR);
	      move (a, arg2EA);
	      move (b, arg1EA);
	      emit_jmpl (opAddrR, zeroRand, linkR);
	      move (arg1EA, b))
	  | intOp _ _ = ErrorMsg.impossible "[SparcCM.intOp]"
	val mulAddrOffset = IMrand 72
	val divAddrOffset = IMrand 76
    in
    val mult = intOp mulAddrOffset
    val divt = intOp divAddrOffset
    end (* local *)

  (* bbs (i, dst, lab): test the i'th bit of dst and jump to lab if it is zero *)
    fun bbs (Immed i, Direct r, ImmedLab lab) = (
	  emit_andcc (r, IMrand(Bits.lshift(1, i)), zeroR);
	  emit_bcc (CC_NE, lab))
      | bbs _ = ErrorMsg.impossible "[SparcCM.bbs]"

    local
      fun revCC CC_A = CC_A
	| revCC CC_E = CC_E     | revCC CC_NE = CC_NE
	| revCC CC_L = CC_G     | revCC CC_LE = CC_GE
	| revCC CC_G = CC_L     | revCC CC_GE = CC_LE
	| revCC CC_LEU = CC_GEU | revCC CC_GEU = CC_LEU
      fun compare (cc, a as Immed _, b as Direct _) = compare (revCC cc, b, a)
	| compare (cc, Direct r1, Direct r2) = (emit_subcc (r1, REGrand r2, zeroR); cc)
	| compare (cc, Direct r1, Immed n) = (compareImmed (r1, n); cc)
	| compare _ = ErrorMsg.impossible "[SparcCM.compare]"
    in

  (* ibranch (cond, a, b, lab): if (a <cond> b) then pc <- lab *)
    fun ibranch (cond, a, b, ImmedLab lab) = emit_bcc (compare (sparcCC cond, a, b), lab)

  (* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
    fun rangeChk (a, b, ImmedLab lab) = emit_bcc (compare(CC_GEU, a, b), lab)

    end (* local *)


    (*
     * Floating point arithmetic instructions
     *)
    local
    (* Fetch a ML real value into a floating-point register pair *)
      fun fetchReal (Direct r, FREG i) = (
	    emit_ldf (r, zeroRand, FREG i);
	    emit_ldf (r, IMrand 4, FREG(i+1)))
	| fetchReal (ImmedLab lab, dst) = let val tmpR = getTmpReg()
	    in
	      loadF (lab, 0, dst, tmpR);
	      freeTmpReg tmpR
	    end
	| fetchReal _ = ErrorMsg.impossible "[SparcCM.fetchReal]"
      fun floatOp fOp (FDirect fpr1, FDirect fpr2, FDirect fpr3) = fOp(fpr1,fpr2,fpr3)
	| floatOp _ _ = ErrorMsg.impossible "[SparcCM.floatOp]"
    in

    fun loadfloat (src, FDirect fpr) = fetchReal(src, fpr)
      | loadfloat _ = ErrorMsg.impossible "[SparcCM.loadfloat]"

    fun storefloat (FDirect(FREG fpr), Direct(REG gpr)) = let val tmpR = getTmpReg()
	  in
	    loadImmed (System.Tags.desc_reald, tmpR);
	    emit_st (dataptrR, IMrand(~4), tmpR);
	    emit_stf (dataptrR, zeroRand, FREG fpr);
	    emit_stf (dataptrR, IMrand 4, FREG (fpr+1));
	    emitMove (dataptrR, REG gpr);
	    emit_add (dataptrR, IMrand 12, dataptrR);
	    freeTmpReg tmpR
	  end
      | storefloat _ = ErrorMsg.impossible "[SparcCM.storefloat]"

    val faddd = floatOp emit_fadd
    val fsubd = floatOp emit_fsub
    val fmuld = floatOp emit_fmul
    val fdivd = floatOp emit_fdiv

    fun fnegd (FDirect (fpr1 as FREG f1), FDirect (fpr2 as FREG f2)) = (
	  emit_fneg (fpr1, fpr2);
	  if (fpr1 <> fpr2) then emit_fmov (FREG(f1+1), FREG(f2+1)) else ())
      | fnegd _ = ErrorMsg.impossible "[SparcCM.fnegd]"

    fun fabsd (FDirect (fpr1 as FREG f1), FDirect (fpr2 as FREG f2)) = (
	  emit_fabs (fpr1, fpr2);
	  if (fpr1 <> fpr2) then emit_fmov (FREG(f1+1), FREG(f2+1)) else ())
      | fabsd _ = ErrorMsg.impossible "[SparcCM.fabsd]"

  (* convert an int to a double.  Because there is no data-path from general
   * purpose registers to the FP registers, we use the heap as a staging point.
   *)
    local
      val cvti2dAddrOffset = IMrand 92
      fun convert (gpr, fpr) = (
	    emit_st (spR, cvti2dAddrOffset, gpr);
            emit_ldf (spR, cvti2dAddrOffset, fpr);
	    emit_fitod (fpr, fpr))
    in
    fun cvti2d (Direct r, FDirect fpr) = convert (r, fpr)
      | cvti2d (Immed i, FDirect fpr) = let val tmpR = getTmpReg()
	  in
	    loadImmed (i, tmpR);
	    convert (tmpR, fpr);
	    freeTmpReg tmpR
	  end
      | cvti2d _ = ErrorMsg.impossible "[SparcCM.cvti2d]"
    end (* local fun convert ... *)

    fun fbranchd (cond, FDirect fp1, FDirect fp2, ImmedLab lab) = (
	  emit_fcmp(fp1,fp2);
	  emit_fbcc(sparcCC cond, lab))
      | fbranchd _ = ErrorMsg.impossible "[SparcCM.fbranchd]"
    end (* local *)

    end (* local *)

  end (* functor SparcCM *)
