(* i386ascode.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *)

structure I386Assem = struct val outfile = ref(IO.std_out) end

functor I386AsCode() : I386CODER = struct

open I386Assem

type Label = string

datatype Size = Byte | Word | Long

datatype EA = Direct of int
	    | Displace of int * int
	    | Index of int * int * int * Size
	    | Immedlab of Label
	    | Immed of int
	    | Floatreg of int

val eax = 0
and ebx = 3
and ecx = 1
and edx = 2
and esi = 6
and edi = 7
and ebp = 5
and esp = 4

fun die s = ErrorMsg.impossible ("i386/i386ascode.sml: " ^ s)

val offset = ref 0

local val i = ref 0 in
fun newlabel () = (i := !i + 1; "L" ^ makestring (!i))
end

fun itoa (i:int) = if i < 0 then "-" ^ makestring (~i)
		   else makestring i


fun emit s = outputc (!outfile) s

fun emitreg (0) = emit "%eax"
  | emitreg (1) = emit "%ecx"
  | emitreg (2) = emit "%edx"
  | emitreg (3) = emit "%ebx"
  | emitreg (4) = emit "%esp"
  | emitreg (5) = emit "%ebp"
  | emitreg (6) = emit "%esi"
  | emitreg (7) = emit "%edi"
  | emitreg _ = die "emitreg: bad register number"

fun sizeint i =
	if i < 128 andalso i > ~129 then Byte
	else if i < 32768 andalso i > ~32769 then Word
	else Long

fun emitarg (Immed i) = emit ("$" ^ itoa i)
  | emitarg (Direct r) = emitreg r
  | emitarg (Displace (r,i)) = 
	(emit (itoa i ^ "(");
	 emitreg r;
	 emit ")")
  | emitarg (Index (ra,disp,r,s)) =
	(emit (itoa disp ^ "(");
	 emitreg ra;
	 emit ",";
	 emitreg r;
	 emit ",";
	 emit (case s of Byte => "1)" | Word => "2)" | Long => "4)"))
  | emitarg (Immedlab l) = emit l
  | emitarg (Floatreg 0) = emit "%st"
  | emitarg (Floatreg i) = emit ("%st(" ^ itoa i ^ ")")

fun emit2arg (a,b) = (emit "\t"; emitarg a; emit ", "; emitarg b; emit "\n")
fun emit1arg a = (emit "\t"; emitarg a; emit "\n")

fun oct i = let val m = Integer.makestring
	    in	m(i div 64)^m((i div 8)mod 8)^m(i mod 8) end
fun c_char "\n" = "\\n"
  | c_char "\t" = "\\t"
  | c_char "\\" = "\\\\"
  | c_char "\"" = "\\\""
  | c_char c = if ord c < 32 then "\\"^oct(ord c) else c
fun a_str s = implode(map c_char (explode s))

(**************************** Misc. functions ********************************)

fun align () = emit "\t.align 2\n"

fun mark () = let val lab = newlabel()
	      in  emit lab;
		  emit ":\t.long\tMAKE_DESC((";
		  emit lab;
		  emit "-base)/4+1,TAG_backptr)\n"   (* STRING dependency *)
	      end

fun define lab = (emit lab; emit ":")
fun comment s  = (emit "\t# "; emit s)
fun finish ()  = ""

(******************************** Emitters ***********************************)

fun emitstring s = (emit "\t.ascii \""; emit(a_str s); emit "\"\n")
fun realconst s = (emit "\t.float "; emit s; emit "\n")
fun emitlong (i : int) = (emit "\t.long "; emit(makestring i); emit "\n")

fun emitlab (offset,l2) =
	(emit "@@:\t.long "; emit l2; emit "-@b"; 
	 if offset < 0 then (emit "-"; emit (makestring (~offset)))
		       else (emit "+"; emit (makestring offset));
	 emit "\n")

(**************************** Memory functions *******************************)

fun movl  args = (emit "\tmovl"; emit2arg args)
fun movb  args = (emit "\tmovb"; emit2arg args)
fun movzx args = (emit "\tmovzx"; emit2arg args)
fun stos  arg  = (emit "\tstosl"; emit1arg arg)
fun lea	  args = (emit "\tleal"; emit2arg args)
fun push  arg  = (emit "\tpushl"; emit1arg arg)
fun pop	  arg  = (emit "\tpopl"; emit1arg arg)
fun xchg  args = (emit "\txchgl"; emit2arg args)

(************************ Bitwise operations *********************************)

fun xorl args = (emit "\txorl"; emit2arg args)
fun orl	 args = (emit "\torl"; emit2arg args)
fun notl arg  = (emit "\tnotl"; emit1arg arg)
fun andl args = (emit "\tandl"; emit2arg args)
fun btst args = (emit "\tbtl"; emit2arg args)


(***************************** Arithmetic ************************************)

fun incl  arg  = (emit "\tincl"; emit1arg arg)
fun decl  arg  = (emit "\tdecl"; emit1arg arg)
fun addl  args = (emit "\taddl"; emit2arg args)
fun subl  args = (emit "\tsubl"; emit2arg args)
fun mull  args = (emit "\timull"; emit2arg args)
fun asll  args = (emit "\tsall"; emit2arg args)
fun asrl  args = (emit "\tsarl"; emit2arg args)
fun cmpl  args = (emit "\tcmpl"; emit2arg args)
fun negl  arg  = (emit "\tnegl"; emit1arg arg )
fun divl  arg  = (emit "\tidiv"; emit1arg arg)
fun cdq	  ()   =  emit "\tcdq\n" 

(******************************* Jumps ***************************************)

fun jne arg = (emit "\tjne"; emit1arg arg)
fun jeq arg = (emit "\tjeq"; emit1arg arg)
fun jgt arg = (emit "\tjgt"; emit1arg arg)
fun jge arg = (emit "\tjge"; emit1arg arg)
fun jlt arg = (emit "\tjlt"; emit1arg arg)
fun jle arg = (emit "\tjle"; emit1arg arg)
fun jc	arg = (emit "\tjc"; emit1arg arg)
fun jnc arg = (emit "\tjnc"; emit1arg arg)
fun jls arg = (emit "\tjls"; emit1arg arg)
fun ja	arg = (emit "\tja"; emit1arg arg)
fun jae arg = (emit "\tjea"; emit1arg arg)
fun jb	arg = (emit "\tjb"; emit1arg arg)
fun jbe arg = (emit "\tjbe"; emit1arg arg)

fun jra (arg as (Immedlab lab)) = (emit "\tjmp"; emit1arg arg)
  | jra _ = die "jra: bad arg"

fun jmp (arg as Displace _) = (emit "\tjmp"; emit1arg arg)
  | jmp (arg as Direct _  ) = (emit "\tjmp"; emit1arg arg)
  | jmp _ = die "jmp: bad arg"

(********************** Floating point functions  ****************************)

(* 80387 float operations *)
(* Some src/dest combinations are illegal, but not caught here. *)

fun float2 opr pop args = 
    let val opr = if pop then opr ^ "p" else opr
    in
	emit opr;
	emit2arg args
    end

val fadd  = float2 "fadd"
val fsub  = float2 "fsub"
val fsubr = float2 "fsubr"
val fmul  = float2 "fmul"
val fcom  = float2 "fcom"
val fdiv  = float2 "fdiv"
val fdivr = float2 "fdivr"


fun fst pop arg = (emit "\tfst";
		   if pop then emit "p" else ();
		   emit1arg arg)

fun fld	  arg = (emit "\tfld"; emit1arg arg)
fun fild  arg = (emit "\tfild"; emit1arg arg)
fun fchs  ()  = emit "\tfchs\n"
fun fabs  ()  = emit "\tfabs\n"
fun fstsw ()  = emit "\tfstsw\t%ax\n"

(******************************* Traps ***************************************)

fun sahf () = emit "\tsahf\n"
fun into () = emit "\tinto\n"

end (* structure I386AsCode *)
