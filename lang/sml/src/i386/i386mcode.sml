(* i386mcode.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *)

functor I386MCode (Jumps : I386JUMPS) : I386CODER = struct

structure Emitter : BACKPATCH = Backpatch(Jumps)
open Emitter Jumps

val emitbyte = fn i => emitstring(ebyte i)
val emitlong = fn i => emitstring(elong i)
fun realconst s = emitstring(implode(rev(explode(IEEEReal.realconst s))))

datatype EA = Direct of int
	    | Displace of int * int
	    | Index of int * int * int * Size
	    | Immed of int
	    | Immedlab of Label
	    | Floatreg of int

(*************************** The 80386 registers ******************************)

val eax = 0
and ebx = 3
and ecx = 1
and edx = 2
and esi = 6
and edi = 7
and ebp = 5
and esp = 4

(*********************** Emit the addr. and data extension *******************)

fun die s = ErrorMsg.impossible ("i386/i386mcode.sml: " ^ s)

(* Emit the Scaled/Index/Base byte *)
fun emitsib(Index(base, _, index, size)) =
	let val ss = if index=4 then 0
		     else (case size of Byte => 0 | Word => 1 | Long => 2)
	in  ebyte(ss*64 + index*8 + base) end
  | emitsib _ = die "emitsib: bad args"

(* Emit the mod-reg-r/m byte and addr. and data 
 * extension for binary operations 
 *)
fun emitext(Direct s, Direct d) = ebyte(3*64 + 8*d + s)
  | emitext(Displace(s, 0), b as Direct d) =
	if s=esp
	then emitext(Index(s,0,4,Byte), b)
	else	if s=ebp 
		then (ebyte(1*64 + d*8 + ebp) ^ ebyte(0))
		else  ebyte(d*8 + s)
  | emitext(Displace(s,i), b as Direct d) =
	if s=esp
	then emitext(Index(s,i,4,Byte), b)
	else	if sizeint(i)=Byte
		then (ebyte(1*64 + d*8 + s) ^ ebyte(i))
		else (ebyte(2*64 + d*8 + s) ^ elong(i))
  | emitext(src as Index(s, 0,_,_), Direct d) =
	if s=ebp
	then (ebyte(1*64 + 8*d + 4) ^ emitsib(src) ^ ebyte(0))
	else (ebyte(8*d + 4) ^ emitsib(src))
  | emitext(src as Index(_,i,_,_), Direct d) =
	if sizeint(i)=Byte
	then (ebyte(1*64 + d*8 + 4) ^ emitsib(src) ^ ebyte(i))
	else (ebyte(2*64 + d*8 + 4) ^ emitsib(src) ^ elong(i))
  | emitext(a as Direct _, b as Displace _) = emitext(b,a)
  | emitext(a as Direct _, b as Index _) = emitext(b,a)
  | emitext _ = die "emitext: bad args"

fun emitimm i = if sizeint(i)=Byte then ebyte(i) else elong(i)

(* Emit the mod-reg-r/m byte and addr. and data	 extension for 
 * immediate operations. This is also used in unary operations
 *)
fun emitImmext(opcode, Direct r) = ebyte(3*64 + opcode*8 +r)
  | emitImmext(opcode, Displace(r, 0)) =
	if r=esp 
	then emitImmext(opcode, Index(r,0,4,Byte))
	else	if r=ebp
		then (ebyte(1*64 + opcode*8 + 5) ^ ebyte(0))
		else ebyte(opcode*8 + r)
  | emitImmext(opcode, Displace(r, j)) =
	if r=esp
	then emitImmext(opcode, Index(r,j,4,Byte))
	else	let val mode = (if (sizeint(j) = Byte) then 1 else 2)
		in
		    (ebyte(mode*64 + opcode*8 + r) ^ emitimm(j))
		end
  | emitImmext(opcode, dest as Index(r, 0, _, _)) =
	if r=ebp
	then (ebyte(1*64 + opcode*8 + 4) ^ emitsib(dest) ^ ebyte(0))
	else (ebyte(opcode*8 + 4) ^ emitsib(dest))
  | emitImmext(opcode, dest as Index(b, j, _, _)) =
	let val mode = (if (sizeint(j) = Byte) then 1 else 2)
	in (ebyte(mode*64 + opcode*8 + 4) ^ emitsib(dest) ^ emitimm(j))
	end
  | emitImmext _ = die "emitImmext: bad args"

(* Generate code for binary operations *)
(******
fun gen2(frst,nxt, src, dest) =
	(case (src,dest) of
	    (Immed i, _)  =>  if ~128<=i andalso i<128
			      then (ebyte(131) ^
				    emitImmext(nxt,dest) ^
				    ebyte(i))
			      else (ebyte(129) ^
				    emitImmext(nxt,dest) ^
				    elong(i))
	  | (_, Direct _) => (ebyte(frst+3) ^ emitext(src, dest))
	  | (Direct _, _) => (ebyte(frst+1) ^ emitext(src, dest))
	  | _ => die "gen2: bad args")
******)

fun gen2(frst,nxt, src, dest) =
	(case (src,dest) of
	    (Immed i, _)  =>  
		if sizeint(i) = Byte
		    then (ebyte(131) ^ emitImmext(nxt,dest) ^ ebyte(i))
		else if dest = Direct 0
		    then ebyte (8 * nxt + 5) ^ elong i
		else ebyte(129) ^ emitImmext(nxt,dest) ^ elong(i)
	  | (_, Direct _) => (ebyte(frst+3) ^ emitext(src, dest))
	  | (Direct _, _) => (ebyte(frst+1) ^ emitext(src, dest))
	  | _ => die "gen2: bad args")

fun incl(x as Direct d)	  = emitstring(ebyte(64+d))
  | incl(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(0,x))
  | incl(x as Index _)	  = emitstring(ebyte(255) ^ emitImmext(0,x))
  | incl _ = die "incl: bad args"

fun decl(x as Direct d)	  = emitstring(ebyte(72+d))
  | decl(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(1,x))
  | decl(x as Index _)	  = emitstring(ebyte(255) ^ emitImmext(1,x))
  | decl _ = die "decl: bad args"

fun addl(Immed 1, dest) = incl(dest)
  | addl(src, dest)	= emitstring(gen2(  0, 0, src, dest))

fun subl(Immed 1, dest) = decl(dest)
  | subl(src, dest)	= emitstring(gen2( 40, 5, src, dest))

fun orl (src, dest) = emitstring(gen2(	8, 1, src, dest))
fun xorl(src, dest) = emitstring(gen2( 48, 6, src, dest))
fun andl(src, dest) = emitstring(gen2( 32, 4, src, dest))
fun cmpl(src, dest) = emitstring(gen2( 56, 7, src, dest))

fun xchg(Direct 0, Direct r) = emitstring(ebyte(144+r))
  | xchg(Direct r, Direct 0) = emitstring(ebyte(144+r))
  | xchg(x, y) = emitstring(ebyte(135) ^ emitext(x,y))

fun notl(x as Direct _) = emitstring(ebyte(247) ^ emitImmext(2,x))
  | notl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(2,x))
  | notl _ = die "notl: bad args"

fun negl(x as Direct _) = emitstring(ebyte(247) ^ emitImmext(3,x))
  | negl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(3,x))
  | negl _ = die "negl: bad args"

fun movl(Immed i, Direct r) = 
	emitstring(ebyte(184+r) ^ elong(i))
  | movl(Immed i, dest) = 
	emitstring(ebyte(199) ^ emitImmext(0,dest) ^ elong(i))
  | movl(src, dest) = emitstring(gen2(136, 0, src, dest))

fun movb(Immed i, y) = 
       if sizeint i <> Byte 
	   then die "movb: immediate value is not byte-sized"
       else emitstring (ebyte 198 ^ emitImmext(0, y) ^ ebyte i)
  | movb(x, y as Direct y')  = if y' > 3 then die "movb: bad register"
			       else emitstring(ebyte(138) ^ emitext(x,y))
  | movb(x as Direct x', y) = if x' > 3 then die "movb: bad register"
			      else emitstring(ebyte(136) ^ emitext(x,y))
  | movb _ = die "movb: bad args"

fun movzx(x, y as Direct _) = emitstring(ebyte(15) ^ ebyte(182) ^ emitext(x,y))
  | movzx _ = die "movzx: bad args"

fun stos(Direct 0) = emitstring(ebyte(171))
  | stos _ = die "stos: bad args"

fun push(Direct d) = emitstring(ebyte(80 + d))
  | push _ = die "push: bad args"

fun pop(Direct d) = emitstring(ebyte(88 + d)) 
  | pop _ = die "pop: bad args"

fun shift(_,Immed 0, _) = ()
  | shift(TTT, Immed 1, dest) = 
	emitstring(ebyte(209) ^ emitImmext(TTT,dest))
  | shift(TTT, cnt as Immed i, dest) = 
	emitstring(ebyte(193) ^ emitImmext(TTT,dest) ^ ebyte(i))
  | shift(TTT, cnt as Direct 1, dest) = 
	emitstring(ebyte(211) ^ emitImmext(TTT,dest))
  | shift _ = die "shift: bad args"

fun asll(cnt, dest) = shift(4, cnt, dest)
fun asrl(cnt, dest) = shift(7, cnt, dest)

(****
fun lea(Displace(s, 0),Direct r) =
	emitstring(ebyte(139) ^ ebyte(3*64 + 8*r + s))
  | lea(Displace(s, i),Direct r) = emitstring(
	ebyte(141) ^
	(case sizeint(i) of
	    Byte => (ebyte(1*64 + 8*r + s) ^ ebyte(i))
	  | _	 => (ebyte(2*64 + 8*r + s) ^ elong(i))))
  | lea(Immedlab l, Direct r) = jump(LEA(r), l)
  | lea _ = die "lea: bad args"
****)

fun lea(Displace(s, 0), Direct d) = movl (Direct s, Direct d)
  | lea(s as Displace _, d as Direct _) = emitstring(ebyte(141) ^ emitext(s,d))
  | lea(s as Index _, d as Direct _) = emitstring(ebyte(141) ^ emitext(s,d))
  | lea(Immedlab l, Direct d) = jump(LEA(d), l)
  | lea _ = die "lea: bad args"

fun btst(src as Immed i, dst as Direct _) = emitstring(
	ebyte(15) ^
	ebyte(186) ^ 
	emitImmext(4,dst) ^
	ebyte(i) )
  | btst(src as Immed i, dst as Displace _) = emitstring(
	ebyte(15) ^
	ebyte(186) ^
	emitImmext(4,dst) ^
	ebyte(i) )
  | btst _ = die "btst: bad args"

fun emitlab(i,lab) = jump(LABPTR(i), lab)

local fun jcc i (Immedlab lab) = jump (Jcc i, lab)
	| jcc _ _ = die "jcc: bad args"
in
    val jne = jcc 5
    val jeq = jcc 4
    val jgt = jcc 15
    val jge = jcc 13
    val jlt = jcc 12
    val jle = jcc 14
    val jb  = jcc 2
    val jbe = jcc 6
    val ja  = jcc 7
    val jae = jcc 3
    val jc  = jcc 2
    val jnc = jcc 3
end

fun jra(arg as Immedlab lab) = jump(JMP, lab)
  | jra _ = die "jra: bad args"

fun jmp(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(4,x))
  | jmp(x as Direct _)	 = emitstring(ebyte(255) ^ emitImmext(4,x))
  | jmp _ = die "jmp: bad args"

(****
fun mull(x as Direct _, y as Direct _) = emitstring(
	ebyte(15) ^
	ebyte(175) ^
	emitext(x,y))
  | mull _ = die "mull: bad args"
****)

fun mull(Immed i, Direct r) =
       if sizeint(i) = Byte 
	   then emitstring(ebyte(107) ^ ebyte(3*64 + 8*r + r) ^ ebyte(i))
       else
	   emitstring(ebyte(105) ^ ebyte(3*64 + 8*r + r) ^ elong(i))
  | mull(src, dest as Direct _) = 
       emitstring(ebyte(15) ^ ebyte(175) ^ emitext(src,dest))
  | mull _ = die "mull: bad args"

fun divl(x as Direct r) =  emitstring(ebyte(247) ^ emitImmext(7,x)) 
  | divl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(7,x))
  | divl _ = die "divl: bad args"

fun cdq() = emitstring(ebyte(153))

(******************** Floating point operations *******************)

(* Instead of using separate functions for those operations that pop
   the 80387 stack (e.g. faddp, fstp, etc.), these functions take a
   boolean argument that specifies whether to pop. *)

(* floatarith() emits an arithmetic floating point instruction (e.g.,
   fadd, fmul, etc.)  The operation is encoded in the REG field of the
   MOD/RM byte, which is generated by emitext().  These instructions
   are binary, but one of the arguments must be the top of the
   register stack.  If the destination is the the top of the stack, the
   instruction cannot pop. *)

fun float_arith opr true (Floatreg 0, Floatreg r) = 
       emitstring (ebyte 0xde ^ emitext (Direct r, Direct opr))
  | float_arith opr false (Floatreg 0, Floatreg r) = 
       emitstring (ebyte 0xdc ^ emitext (Direct r, Direct opr))
  | float_arith opr false (Floatreg r, Floatreg 0) =
       emitstring (ebyte 0xd8 ^ emitext (Direct r, Direct opr))
  | float_arith opr false (src as Displace _, Floatreg 0) =
       emitstring (ebyte 0xdc ^ emitext (src, Direct opr))
  | float_arith _ _ _ = die "float_arith: bad args"

val fadd  = float_arith 0
val fmul  = float_arith 1
val fcom  = fn pop => if pop then float_arith 3 false
		      else float_arith 2 false
val fsub  = float_arith 4
val fsubr = float_arith 5
val fdiv  = float_arith 6
val fdivr = float_arith 7

fun fchs ()  = emitstring (ebyte 0xd9 ^ ebyte 0xe0)
fun fabs ()  = emitstring (ebyte 0xd9 ^ ebyte 0xe1)
fun fstsw () = emitstring (ebyte 0xdf ^ ebyte 0xe0)

fun fld (Floatreg r) = 
       emitstring (ebyte 0xd9 ^ emitext (Direct r, Direct 0))
  | fld (src as Displace _) = 
       emitstring (ebyte 0xdd ^ emitext (src, Direct 0))
  | fld (src as Index _) = 
       emitstring (ebyte 0xdd ^ emitext (src, Direct 0))
  | fld _ = die "fld: bad args"

fun fild (src as Displace _) = 
       emitstring (ebyte 0xdb ^ emitext (src, Direct 0))
  | fild (src as Index _) = 
       emitstring (ebyte 0xdb ^ emitext (src, Direct 0))
  | fild _ = die "fild: bad args"

fun fst pop dst =
    let val opr = if pop then 3 else 2
    in
	emitstring (ebyte 0xdd);
	case dst
	  of Floatreg r => emitstring (emitext (Direct r, Direct opr))
	   | Displace _ => emitstring (emitext (dst, Direct opr))
	   | Index _ => emitstring (emitext (dst, Direct opr))
	   | _ => die "fst: bad args"
    end

(********************* Misc. Functions *********************)

fun sahf() = emitstring(ebyte(158))

fun into () = emitstring(ebyte(206))

fun comment _ = ()

val finish = Emitter.finish

end (* functor I386MCode *)
