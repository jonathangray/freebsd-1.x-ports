(** IBM RS6000 machine code generator **)
structure KeepRS6000MCode : sig
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
	
structure RS6000MCodeEmitter : EMITTER = 
struct

  structure M = RS6000InstrSet 
  structure K = KeepRS6000MCode
  open M

  fun error msg = ErrorMsg.impossible ("RS6000MCodeEmitter." ^ msg)

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

  fun emitHiLo(hi,lo) = ( emitByte ((hi >> 8) & 255);
			  emitByte (hi & 255);
			  emitByte ((lo >> 8) & 255);
			  emitByte (lo & 255))


  fun emitLong n = emitHiLo((n >> 16) & 65535, n & 65535)

  fun emitString s = let
	fun copy i = (emitByte(ordof(s, i)); copy(i+1))
	in
	  (copy 0) handle Ord => ()
	end

  exception BadReal = IEEEReal.BadReal
  val emitReal = emitString o IEEEReal.realconst

  fun emitAddr (INFO{addrOf,...}) (lab,k) = emitLong (k + addrOf lab - !loc)

  fun define _ _ = ()

  local open System.Tags
  in
      fun mark() = emitLong(make_desc((!loc + 4)>>2,tag_backptr))
  end

  fun comment _ = ()

  fun emitInstr info = let
      val don'tCare = 0
      fun d_form(opcd,rt,ra,si) = let 
	  val hi = (opcd << 10) || (rt << 5) || ra
	  val lo = si
	in 
	    emitHiLo(hi,lo)
	end

      fun b_form(opcd,bo,bi,bd,aa,lk) = let
	  val hi = (opcd << 10) || (bo << 5) || bi
	  val lo = (bd << 2) || (aa << 1) || lk
	in
	    emitHiLo(hi,lo)
	end

      fun i_form(opcd,li,aa,lk) = let
	  val liLo = li & 0x3fff
	  val liHi = (li >> 14) & 0x3ff
	  val hi = (opcd << 10) || liHi
	  val lo = (liLo << 2) || (aa << 1) || lk
	in
	    emitHiLo(hi,lo)
	end

      fun x_form(opcd,rt,ra,rb,eo,rc) = let
	  val hi = (opcd << 10) || (rt <<5) || ra
	  val lo = (rb << 11) || (eo << 1) || rc
	in
	    emitHiLo(hi,lo)
	end

      fun xl_form(opcd,bt,ba,bb,eo,lk) = let
	  val hi = (opcd << 10) || (bt << 5) || ba
	  val lo = (bb << 11) || (eo << 1) || lk
	in
	    emitHiLo(hi,lo)
	end

      fun xo_form(opcd,rt,ra,rb,oe,eo',rc) = let
	  val hi = (opcd << 10) || (rt << 5) || ra
	  val lo = (rb << 11) || (oe << 10) || (eo' << 1) || rc
	in
	    emitHiLo(hi,lo)
	end

      fun a_form(opcd,frt,fra,frb,frc,xo,rc) = let
	  val hi = (opcd << 10) || (frt << 5) || fra
	  val lo = (frb << 11) || (frc << 6) || (xo << 1) || rc
	in
	    emitHiLo(hi,lo)
	end

      fun m_form(opcd,rs,ra,rb,mb,me,rc) = let
	  val hi = (opcd << 10) || (rs << 5) || ra
	  val lo = (rb << 11) || (mb << 6) || (me << 1) || rc
	in
	    emitHiLo(hi,lo)
	end


      fun cr_bits M.LT = 0
	| cr_bits M.GT = 1
	| cr_bits M.EQ = 2
	| cr_bits M.SO = 3
	| cr_bits _    = error "cr_bits"

      fun fcr_bits(M.FL,2)  = 8
	| fcr_bits(M.FG,2)  = 9
	| fcr_bits(M.FE,2)  = 10
	| fcr_bits(M.FO,2)  = 11
	| fcr_bits(M.FX,1)  = 4
	| fcr_bits(M.FEX,1) = 5
	| fcr_bits(M.VX,1)  = 6
	| fcr_bits(M.OX,1)  = 7
	| fcr_bits _ = error "fcr_bits"

      fun immedLabOff labexp = let  val labOff = M.labelValue info labexp 
			       in
				   (labOff - !loc) div 4
			       end

      fun emitBranchcc (bool,Label16Off labexp,crbit) = let
	  val bo = if bool then 0x0c else 0x4
	  val lab = immedLabOff labexp
	in
	    b_form(16,bo,crbit,lab,0,0)
	end

      fun immed_eaValue ea = let
	  fun immed_eaValue(Immed16Op n) = n
	    | immed_eaValue(LabelOp lab) = M.labelValue info lab
	    | immed_eaValue(HiLabOp lab) = M.hiLabelValue info lab
	    | immed_eaValue(LoLabOp lab) = M.loLabelValue info lab
	    | immed_eaValue _ = error "immed_eaValue"
	  exception TooLarge
	  fun check n = if n >= ~32768 andalso n<65535 then n
			else (app System.Print.say
			       ["immed_eaValue: constant too large ",
				makestring n,"\n"];
			      raise TooLarge)
	in
	    check (immed_eaValue ea)
	end
  in
      fn NOP 				 => error "emitInstr: NOP"
       | B lab24exp => let
           val lab = M.labBranch24Off info lab24exp - (!loc div 4)
	 in 
	     i_form(18,lab,0,0)
	 end
       | BB(cc,bool,lab)		=> emitBranchcc(bool,lab,cr_bits cc)
       | BBF(cc,cr,bool,lab) 		=> emitBranchcc(bool,lab,fcr_bits(cc,cr))
       | BR() 		   	     => xl_form(19,0x14,don'tCare,don'tCare,16,0)
       | LBZ(Reg rt,Reg ra,RegOp(Reg rb))=> x_form(31,rt,ra,rb,87,0)
       | LBZ(Reg rt,Reg ra,ea)   	=> d_form(34,rt,ra,immed_eaValue ea)
       | L(Reg rt,Reg ra,RegOp(Reg rb)) => x_form(31,rt,ra,rb,23,1)
       | L(Reg rt,Reg ra,ea) 	   	=> d_form(32,rt,ra,immed_eaValue ea)
       | LFD(Freg frt,Reg ra,ea) 	=> d_form(50,frt,ra,immed_eaValue ea)
       | LIU(_,RegOp _)			=> error "emitInstr: LIU"
       | LIU(Reg rt,ui) 	   	=> d_form(15,rt,0,immed_eaValue ui)
       | MTSPR(LR,Reg rs) 	        => x_form(31,rs,0x8,don'tCare,467,0)
       | FMR(Freg frt,Freg frb)         => x_form(63,frt,don'tCare,frb,72,0)
       | MTFSB1 bt 	                => x_form(63,bt,don'tCare,don'tCare,38,1)
       | TRAP() 			=> x_form(31,4,0,0,4,0)
       | CAL(Reg rt,Reg ra,RegOp(Reg rb)) => xo_form(31,rt,ra,rb,0,266,0)
       | CAL(Reg rt,Reg ra,ea)		=> d_form(14,rt,ra,immed_eaValue ea)
       | STB(Reg rs,Reg ra,RegOp(Reg rb))=> x_form(31,rs,ra,rb,215,0)
       | STB(Reg rs,Reg ra,ea) 	         => d_form(38,rs,ra,immed_eaValue ea)
       | ST(Reg rs,Reg ra,RegOp(Reg rb)) => x_form(31,rs,ra,rb,151,0)
       | ST(Reg rs,Reg ra,ea)  	         => d_form(36,rs,ra,immed_eaValue ea)
       | STFD(Freg frs,Reg ra,ea) 	 => d_form(54,frs,ra,immed_eaValue ea)

       | A(Reg rt,Reg ra,RegOp(Reg rb))  => xo_form(31,rt,ra,rb,0,10,0)
       | A(Reg rt,Reg ra,ea) 	      	 => d_form(12,rt,ra,immed_eaValue ea)
       | AO(Reg rt,Reg ra,Reg rb) 	 => xo_form(31,rt,ra,rb,1,10,1)
       | FAO(Freg frt,Freg fra,Freg frb) => a_form(63,frt,fra,frb,don'tCare,21,1)

       | SF(Reg rt,Reg ra,RegOp(Reg rb)) => xo_form(31,rt,ra,rb,0,8,0)
       | SF(Reg rt,Reg ra,ea)            => d_form(8,rt,ra,immed_eaValue ea)
       | SFO(Reg rt,Reg ra,Reg rb) 	 => xo_form(31,rt,ra,rb,1,8,1)
       | FSO(Freg frt,Freg fra,Freg frb) => a_form(63,frt,fra,frb,don'tCare,20,1)

       | MULSO(Reg rt,Reg ra,Reg rb) 	 => xo_form(31,rt,ra,rb,1,235,1)
       | FMO(Freg frt,Freg fra,Freg frc) => a_form(63,frt,fra,don'tCare,frc,25,1)

       | DIVS(Reg rt,Reg ra,Reg rb) 	 => xo_form(31,rt,ra,rb,1,363,1)
       | FDO(Freg frt,Freg fra,Freg frb) => a_form(63,frt,fra,frb,don'tCare,18,1)

       | FNEG(Freg frt,Freg frb) 	 => x_form(63,frt,don'tCare,frb,40,1)
       | FABS(Freg frt,Freg frb) 	 => x_form(63,frt,don'tCare,frb,264,1)

       | CMP(Reg ra,RegOp(Reg rb))       => x_form(31,0,ra,rb,0,0)
       | CMP(Reg ra,ea)          	 => d_form(11,0,ra,immed_eaValue ea)
       | CMPL(Reg ra,Reg rb) 		 => x_form(31,0,ra,rb,32,0)

       | FCMP(Freg fra,Freg frb) 	 => x_form(63,2 << 2,fra,frb,32,0)

       | AND(Reg ra,Reg rs,RegOp(Reg rb)) => x_form(31,rs,ra,rb,28,0)
       | AND(Reg ra,Reg rs, ea)  	  => d_form(28,rs,ra,immed_eaValue ea)
       | OR(Reg ra,Reg rs,RegOp(Reg rb))  => x_form(31,rs,ra,rb,444,0)
       | OR(Reg ra,Reg rs,ea) 	          => d_form(24,rs,ra,immed_eaValue ea)
       | XOR(Reg ra,Reg rs,RegOp(Reg rb)) => x_form(31,rs,ra,rb,316,0)
       | XOR(Reg ra,Reg rs,ea) 	          => d_form(26,rs,ra,immed_eaValue ea)
       | XORU(_,_,RegOp _) 		  => error "emitInstr: XORU"
       | XORU(Reg ra,Reg rs,ea)		  => d_form(27,rs,ra,immed_eaValue ea)

       | SL(Reg ra,Reg rs,RegShift(Reg rb))  => x_form(31,rs,ra,rb,24,0)
       | SL(Reg ra,Reg rs,Int5Shift si)      => m_form(21,rs,ra,si,0,31-si,0)
       | SRA(Reg ra,Reg rs,RegShift(Reg rb)) => x_form(31,rs,ra,rb,792,0)
       | SRA(Reg ra,Reg rs,Int5Shift si)     => x_form(31,rs,ra,si,824,0)
       | _ => error "emitInstr"
  end 
end

