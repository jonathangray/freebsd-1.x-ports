(* i386jumps.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *)

structure I386Jumps : I386JUMPS = struct

datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int

datatype Size = Byte | Word | Long

fun die s = ErrorMsg.impossible ("i386/i386jumps.sml: " ^ s)

fun sizeint i =
    if i < 128 andalso i > ~129 then Byte
    else if i < 32768 andalso i > ~32769 then Word
    else Long

fun sizejump (LEA _, _, _, _) = 12
  | sizejump (LABPTR _, _, _, _) = 4
  | sizejump (Jcc _, _, s, d)	 =
	if sizeint(d-s-2)=Byte then 2 else 6
  | sizejump (JMP, _, s, d)    =
	if sizeint(d-s-2)=Byte then 2 else 5

fun signedbyte i = if i<0 then signedbyte (256+i) else i

fun ebyte i = if i>255 then die "ebyte: out of range"
	      else chr(signedbyte i)
fun eword i = chr(Bits.andb(i,255)) ^ chr(Bits.andb(Bits.rshift(i,8),255))
fun elong i = eword(Bits.andb(i,65535)) ^ eword(Bits.rshift(i,16))

val emitlong = elong

fun emitjump (Jcc(cond), 2, s, d) =
	ebyte(112 + cond) ^ ebyte(d-s-2)
  | emitjump (Jcc(cond), 6, s, d) =
	ebyte(15) ^ ebyte(128 + cond) ^ elong(d-s-6)
  | emitjump (JMP, 2, s, d) = ebyte(235) ^ ebyte(d-s-2)
  | emitjump (JMP, 5, s, d) = ebyte(233) ^ elong(d-s-5)
  | emitjump (LABPTR i, 4, s, d) = elong(d-s+i)
  | emitjump (LEA(r), _, s, d) =
	ebyte(232) ^ elong(0) ^				(* call relative 0   *)
	ebyte(88 + r) ^					(* pop r	     *)
	ebyte(129) ^ ebyte(192 + r) ^ elong(d-s-5)	(* add r,(d-s-5)     *)
  | emitjump _ = die "emitjump: bad arg"



end (* structure I386Jumps *)
